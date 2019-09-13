# build_and_validate_model -----------------------------------------------------

#' @importFrom rlang .data
#' @keywords internal
build_and_validate_model <- function(river_data, river, n_folds = 5)
{
  ### Anwenden von calc_t() auf Inputliste
  river_data_ts <- lapply(river_data, prepare_river_data)

  # step through, forward and backward selection
  fb <- get_forward_backward(river_data_ts, river)

  ### Validation ###

  # Test for classical statistical model assumptions, normality of residuals
  # and heteroskelasdicity
  stat_tests <- init_stat_tests_data(fb)

  # Create list of independent training rows
  train_rows <- get_training_rows(
    model_object = kwb.utils::selectElements(fb, paste0(river, "model_01")),
    n_folds = n_folds
  )

  stat_tests <- update_stat_tests(stat_tests, fb, train_rows)

  sorted_models <- stat_tests %>%
    dplyr::filter(.data$below95 == 5 & .data$below90 == 5 & .data$in50 == 5) %>%
    dplyr::arrange(dplyr::desc(.data$R2))

  # Select the best model from fb
  best_model <- kwb.utils::selectElements(fb, sorted_models$model[1])

  stanfit <- rstanarm::stan_glm(
    formula = get_formula(best_model),
    data = kwb.utils::selectElements(best_model, "model")
  )

  list(sorted_models, best_model, stanfit)
}

# prepare_river_data -----------------------------------------------------------
prepare_river_data <- function(river_list)
{
  river_ts <- calc_t(datalist = river_list)

  # Names of list elements
  elements <- names(river_ts)

  is_discharge <- grepl("^q",  elements)
  is_treatment <- grepl("^ka", elements)
  is_i         <- grepl("^i",  elements)
  is_rain      <- grepl("^r",  elements)

  river_ts[is_discharge] <- lapply(river_ts[is_discharge], add_meancol)
  river_ts[is_treatment] <- lapply(river_ts[is_treatment], add_meancol)
  river_ts[is_i]         <- lapply(river_ts[is_i],         add_meancol)
  river_ts[is_rain]      <- lapply(river_ts[is_rain],      add_meancol)

  river_ts
}

# calc_t -----------------------------------------------------------------------
calc_t <- function(datalist)
{
  # Filter for summer months in the hygienic data
  hygiene_element <- grep("hygiene", names(datalist), value = TRUE)
  stopifnot(length(hygiene_element) == 1)

  hygienic <- kwb.utils::selectElements(datalist, hygiene_element)

  # Data frames with non-hygienic data
  non_hygienics <- datalist[setdiff(names(datalist), hygiene_element)]

  # Filter hygienic measurements for months in summer (May to September)
  hygienic_summer <- filter_for_months(hygienic, 5:9)

  # Filter non-hygienic measurements for months in summer (April to September)
  non_hygienics_summer <- lapply(non_hygienics, filter_for_months, 4:9)

  # z-transform the data frames with non-hygienic data
  non_hygienics_z <- lapply(non_hygienics_summer, transform_z)

  # Recompose the list of hygienic and non-hygienic data and set original names
  stats::setNames(c(list(hygienic_summer), non_hygienics_z), names(datalist))
}

# filter_for_months ------------------------------------------------------------

# Helper function to filter for month numbers
filter_for_months <- function(df, month_numbers)
{
  dates <- kwb.utils::selectColumns(df, "datum")

  df[lubridate::month(dates) %in% month_numbers, ]
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

# get_value_column_prefixes ----------------------------------------------------
get_value_column_prefixes <- function(df)
{
  unique(sub("([a-z])_.*", "\\1", names(df)[-1]))
}

# get_forward_backward ---------------------------------------------------------
get_forward_backward <- function(river_data_ts, river)
{
  riverdata <- kwb.utils::selectElements(river_data_ts, river)

  fb <- stepwise(riverdata, pattern = "(i_mean|q_mean|r_mean|ka_mean)")

  names(fb) <- sprintf("%smodel_%02d", river, seq_along(fb))

  # Eliminieren von modelled die doppelt vorkommen, da forward selection frueher
  # fertig als n steps
  fb[seq_along(unique(fb))]
}

# stepwise ---------------------------------------------------------------------
stepwise <- function (riverdata, pattern)
{
  # prepare variables out of all cominations (given by pattern)
  # variables for interaction get replaced by q_new (remove q_old)
  vars1 <- (
    riverdata[-1] %>%
      kwb.flusshygiene::unroll_physical_data() %>%
      lapply(names) %>%
      unlist() %>%
      unique()
  )[-1]

  vars2 <- vars1[stringr::str_detect(vars1, pattern)]

  # prepare formulas
  data <- kwb.flusshygiene::process_model_riverdata(
    riverdata, variables = c("log_e.coli", vars1)
  ) %>%
    dplyr::select(- .data$datum)

  # Definition of null and full models
  null <- stats::lm(log_e.coli ~ 1, data = data)
  full <- stats::lm(log_e.coli ~ .^2, data = data)

  # Definition maximum number of steps. 10 at maximum
  max_steps <- min(round(nrow(data) / 10), 10)

  # Creating list of candidate models with 1 ... max_steps predictors
  lapply(X = seq_len(max_steps), FUN = function(steps) {
    stats::step(
      object = null,
      scope = list(upper = full, lower = null),
      direction = "forward",
      steps = steps,
      data = data
    )
  })
}

# init_stat_tests_data ---------------------------------------------------------
#' @importFrom rlang .data
#' @keywords internal
init_stat_tests_data <- function(fb)
{
  sapply(fb, get_stat_tests) %>%
    t() %>%
    dplyr::as_tibble(rownames = "model")  %>%
    dplyr::bind_rows(.id = "river") %>%
    dplyr::mutate(stat_correct = .data$N > 0.05 & .data$BP > 0.05) %>%
    dplyr::mutate(in50 = 0, below90 = 0, below95 = 0, in95 = 0)
}

# get_stat_tests ---------------------------------------------------------------

#' Calculate Statistical Tests for Residuals
#'
#' Normality and s2 = const shapiro-wilk test and breusch-pagan test
#'
#' @param model model object
#'
get_stat_tests <- function(model)
{
  get <- kwb.utils::selectElements

  residuals <- get(model, "residuals")

  c(
    N = get(stats::shapiro.test(residuals), "p.value"),
    get(lmtest::bptest(model), "p.value"),
    R2 = get(summary(model), "adj.r.squared"),
    n_obs = length(residuals)
  )
}

# get_training_rows ------------------------------------------------------------
get_training_rows <- function(model_object, n_folds)
{
  # Create list of independent training rows
  caret::createFolds(
    seq_len(nrow(kwb.utils::selectElements(model_object, "model"))),
    k = n_folds,
    list = TRUE,
    returnTrain = TRUE
  )
}

# get_formula ------------------------------------------------------------------
get_formula <- function(model)
{
  kwb.utils::selectElements(
    as.list(kwb.utils::selectElements(model, "call")), "formula"
  )
}

# update_stat_tests ------------------------------------------------------------
#' @importFrom rlang .data
#' @keywords internal
update_stat_tests <- function(
  stat_tests, fb, train_rows, probs = c(0.025, 0.25, 0.75, 0.9, 0.95, 0.975)
)
{
  for (model_name in names(fb)) {

    model <- kwb.utils::selectElements(fb, model_name)
    model_data <- as.data.frame(kwb.utils::selectElements(model, "model"))

    selected <- kwb.utils::selectColumns(stat_tests, "model") == model_name

    for (rows in train_rows) {

      row_indices <- c(rows)

      training <- model_data[  row_indices, ]
      test     <- model_data[- row_indices, ]

      fit <- rstanarm::stan_glm(formula = get_formula(model), data = training)

      prediction <- rstanarm::posterior_predict(fit, newdata = test)

      df <- apply(prediction, 2, stats::quantile, probs = probs) %>%
        t() %>%
        as.data.frame() %>%
        dplyr::mutate(
          log_e.coli = test$log_e.coli,
          below95 = .data$log_e.coli < .data$`95%`,
          below90 = .data$log_e.coli < .data$`90%`,
          within95 =
            .data$log_e.coli < .data$`97.5%` &
            .data$log_e.coli > .data$`2.5%`,
          within50 =
            .data$log_e.coli < .data$`75%` &
            .data$log_e.coli > .data$`25%`,
        )

      stat_tests$in95[selected] <- stat_tests$in95[selected] +
        test_beta(is_true = df$within95, percentile = 0.95)

      stat_tests$below95[selected] <- stat_tests$below95[selected] +
        test_beta(is_true = df$below95, percentile = 0.95)

      stat_tests$below90[selected] <- stat_tests$below90[selected] +
        test_beta(is_true = df$below90, percentile = 0.90)

      stat_tests$in50[selected] <- stat_tests$in50[selected] +
        test_beta(is_true = df$within50, percentile = 0.50)
    }
  }

  stat_tests
}

# test_beta --------------------------------------------------------------------
test_beta <- function(is_true, percentile)
{
  stats::pbeta(q = percentile,
    shape1 = sum(is_true) + 1,
    shape2 = sum(! is_true) + 1
  ) > 0.05
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
