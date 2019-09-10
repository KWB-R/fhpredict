if (FALSE)
{
  library(magrittr)
  library(tidyverse)
  library(kwb.flusshygiene)

  #### Laden von Testdaten ###################
  rivers <- c("havel")
  river_paths <- kwb.flusshygiene::get_paths()[paste0(rivers, "data")]
  river_data <- lapply(river_paths, kwb.flusshygiene::import_riverdata)
  river <- "havel"
  names(river_data) <- rivers


  #### Anwenden der Hauptfunktion ###################
  results <- build_and_validate_model(river_data = river_data, river = "havel")
}

# build_and_validate_model -----------------------------------------------------
build_and_validate_model <- function(river_data, river)
{
  ### Anwenden von calc_t() auf Inputliste
  river_data_ts <- lapply(river_data, function(river_list) {

    # use function
    river_ts <- calc_t(river_list)

    # Names of list elements
    elements <- names(river_ts)

    is_discharge <- grepl("^q", elements)
    is_treatment <- grepl("^ka", elements)
    is_i <- grepl("^i", elements)
    is_rain <- grepl("^r", elements)

    river_ts[is_discharge] <- lapply(river_ts[is_discharge], add_meancol)
    river_ts[is_treatment] <- lapply(river_ts[is_treatment], add_meancol)
    river_ts[is_i] <- lapply(river_ts[is_i], add_meancol)
    river_ts[is_rain] <- lapply(river_ts[is_rain], add_meancol)

    river_ts
  })

  # step through, forward and backward selection
  # order of pattern, q_old and q_new is important!
  fb <- stepwise(
    riverdata = river_data_ts[[river]],
    pattern = "(i_mean|q_mean|r_mean|ka_mean)"
  )

  names(fb) <- sprintf(paste0(river, "model_%02d"), seq_along(fb))

  ################ Validation ########################

  # calculate statistical tests for residuals: Normality and s2 = const
  # shapiro-wilk test and breusch-pagan test
  get_stat_tests <- function(model) {
    c(N = shapiro.test(model$residuals)$p.value, lmtest::bptest(model)$p.value,
      R2 = summary(model)[["adj.r.squared"]], n_obs = length(model$residuals))
  }

  # Eliminieren von modelled die doppelt vorkommen, da forward selection früher
  #fertig als n steps

  unique_index <- length(unique(fb))
  fb <- fb[1:unique_index]

  # testing for classical statistical model assumtions, normality of residuals and
  # heteroskelasdicity

  river_stat_tests <- sapply(fb, get_stat_tests) %>%
    t() %>%
    dplyr::as_tibble(rownames = "model")  %>%
    dplyr::bind_rows(.id = "river") %>%
    dplyr::mutate(stat_correct = N > .05 & BP > .05)

  # creating list of independent training rows
  train_rows <- caret::createFolds(
    1:nrow(fb[[paste0(river, "model_01")]]$model),
    k = 5, list = T, returnTrain = TRUE
  )

  fmla <- list()

  for (i in names(fb)) {
    fmla[[i]] <- as.list(fb[[i]]$call)$formula
  }

  test_beta <- function(true, false, percentile) {

    pbeta(q = percentile, shape1 = true + 1, shape2 = false + 1) > 0.05
  }

  # Initialise columns
  river_stat_tests$in95 <- 0
  river_stat_tests$below95 <- 0
  river_stat_tests$below90 <- 0
  river_stat_tests$in50 <- 0

  probs <- c(0.025, 0.25, 0.75, 0.9, 0.95, 0.975)

  for(i in names(fb)) {

    for(j in 1:5) {

      training <- as.data.frame(fb[[i]]$model)[c(train_rows[[j]]),]
      test <- as.data.frame(fb[[i]]$model)[-c(train_rows[[j]]),]

      fit <- rstanarm::stan_glm(fmla[[i]], data = training)

      df <- apply(
        X = rstanarm::posterior_predict(fit, newdata = test),
        MARGIN = 2,
        FUN = quantile,
        probs = probs
      ) %>%
        t() %>%
        as.data.frame() %>%
        dplyr::mutate(
          log_e.coli = test$log_e.coli,
          below95 = log_e.coli < `95%`,
          below90 = log_e.coli < `90%`,
          within95 = log_e.coli < `97.5%`& log_e.coli > `2.5%`,
          within50 = log_e.coli < `75%`& log_e.coli > `25%`,
        )

      selected <- river_stat_tests$model == i

      river_stat_tests$in95[selected] <- river_stat_tests$in95[selected] +
        test_beta(
          true = sum(df$within95),
          false = sum(!df$within95),
          percentile = .95
        )

      river_stat_tests$below95[selected] <- river_stat_tests$below95[selected] +
        test_beta(
          true = sum(df$below95),
          false = sum(!df$below95),
          percentile = .95
        )

      river_stat_tests$below90[selected] <- river_stat_tests$below90[selected] +
        test_beta(
          true = sum(df$below90),
          false = sum(!df$below90),
          percentile = .90
        )

      river_stat_tests$in50[selected] <- river_stat_tests$in50[selected] +
        test_beta(
          true = sum(df$within50),
          false = sum(!df$within50),
          .5
        )
    }
  }

  sorted_modellist <- river_stat_tests %>%
    filter( below95 == 5 & below90 == 5& in50==5) %>%
    dplyr::arrange(desc(R2))

  best_valid_model_stats <- sorted_modellist[1, ]
  best_valid_model <- fb[[best_valid_model_stats$model]]

  stanfit <- rstanarm::stan_glm(
    formula = fmla[[best_valid_model_stats$model]],
    data = best_valid_model$model
  )

  list(sorted_modellist, best_valid_model, stanfit)
}

# stepwise ---------------------------------------------------------------------
stepwise <- function (riverdata, pattern)
{
  # prepare variables out of all cominations (given by pattern)
  # variables for interaction get replaced by q_new (remove q_old)
  vars1 <- (
    riverdata[-1] %>%
      unroll_physical_data() %>%
      lapply(names) %>%
      unlist() %>%
      unique()
  )[-1]

  vars2 <- vars1[stringr::str_detect(vars1, pattern)]

  # prepare formulas
  data <- kwb.flusshygiene::process_model_riverdata(
    riverdata, variables = c("log_e.coli", vars1)
  ) %>%
    dplyr::select(-datum)

  # Definition of null and full models
  null <- lm(log_e.coli ~ 1, data = data)
  full <- lm(log_e.coli ~ .^2, data = data)

  # Definition maximum number of steps. 10 at maximum
  nsteps <- min(round(nrow(data) / 10), 10)

  selection <- list()

  # Creating list of candidate models with 1 ...n predictors
  for (i in 1:nsteps) {

    selection[[i]] <- step(
      null,
      data = data,
      direction = "forward",
      list(lower = null, upper = full),
      steps = i
    )
  }

  selection
}

# calc_t -----------------------------------------------------------------------
calc_t <- function(datalist)
{
  # Helper function to filter for month numbers
  filter_for_months <- function(df, month_numbers) {
    dates <- kwb.utils::selectColumns(df, "datum")
    df[lubridate::month(dates) %in% month_numbers, ]
  }

  # Filter for summer months in the hygienic data
  hygienic <- kwb.utils::selectElements(datalist, "hygiene")

  # Data frames with non-hygienic data
  non_hygienics <- datalist[setdiff(names(datalist), "hygiene")]

  # Filter hygienic measurements for months in summer (May to September)
  hygienic_summer <- filter_for_months(hygienic, 5:9)

  # Filter non-hygienic measurements for months in summer (April to September)
  non_hygienics_summer <- lapply(non_hygienics, filter_for_months, 4:9)

  # z-transform the data frames with non-hygienic data
  non_hygienics_z <- lapply(non_hygienics_summer, transform_z)

  # Recompose the list of hygienic and non-hygienic data and set original names
  stats::setNames(c(list(hygienic), non_hygienics_z), names(datalist))
}

# transform_z ------------------------------------------------------------------
transform_z <- function(df)
{
  # Are the columns rain columns?
  is_rain_column <- grepl("^r_.*", names(df))

  # Transform rain columns: log-transformed and 1/sigma2 (?)
  df[is_rain_column] <- lapply(df[is_rain_column], function(x) log(x + 1))

  # Return the data frame with rain columns being transformed
  df
}

# add_meancol ------------------------------------------------------------------
add_meancol <- function(df)
{
  # for rain and i #edit: + ka #2ndedit: + q
  for (prefix in get_value_column_prefixes(df)) {

    values <- df[, startsWith(names(df), prefix), drop = FALSE]

    df[, paste0(prefix,"_mean")] <- rowMeans(values, na.rm = TRUE)
  }

  df
}

# add_sumcol -------------------------------------------------------------------
add_sumcol <- function (df)
{
  # originally for ka, but not used
  prefix <- get_value_column_prefixes(df)

  if (length(df) > 2) {

    values <- df[, -1, drop = FALSE]

    df[[paste0(prefix, "_sum")]] <- rowSums(values, na.rm = TRUE)
  }

  df
}

# get_value_column_prefixes ----------------------------------------------------
get_value_column_prefixes <- function(df)
{
  unique(sub("([a-z])_.*", "\\1", names(df)[-1]))
}
