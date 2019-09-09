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
build_and_validate_model <- function(river_data, river) {

calc_t <- function (datalist=river_data$havel) {
  phy_data <- datalist[-1] # Entfernung der Hygienedaten
  hyg_df <- subset(datalist[[1]],
                   subset = lubridate::month(datum) %in% 5:9) # Filtern nach Sommer
  data_summer <- lapply(phy_data, function(df){
    df <- subset(df, subset = lubridate::month(datum) %in% 4:9)
  })

  # z_standardize <- function (x) {
  #   y = (x - mean(x, na.rm=T))/sd(x, na.rm=T)
  # }
  transform_z <- function(df) {
    for (site in names(df)[-1]) { # every col gets treatment
      df2 <- subset(df, select = c("datum", site))
      if (grepl("^r_.*",site)) { # rain gets log-transformed and 1/sigma2
        df2[[site]] <- log(df2[[site]]+1)
        # df2[[site]] <- df2[[site]]/sd(df2[[site]], na.rm=T)
      } #else {
      #   df[[site]] <- z_standardize(df2[[site]]) # standardize
      # }
      df[[site]] <- df2[[site]]
    }
    return(df)
  }
  data_t <- lapply(data_summer, transform_z)
  result <- append(list(hyg_df), data_t)
  names(result) <- names(datalist)
  return(result)
}

### Anwenden von calc_t auf Inputliste
river_data_ts <- lapply(river_data, function(river_list){
  river_ts <- calc_t(river_list) # use function
  add_meancol <- function (df) { # for rain and i #edit: + ka #2ndedit: + q
    prefix <- unique(sub("([a-z])_.*","\\1",names(df)[-1]))
    for (pre in prefix) {
      df2 <- dplyr::select(df, dplyr::starts_with(pre))
      df[,paste0(pre,"_mean")] <- rowMeans(df2, na.rm=T)
    }

    return(df)
  }
  add_sumcol <- function (df) { # originally for ka, but not used
    prefix <- unique(sub("([a-z])_.*","\\1",names(df)[-1]))
    if (length(df) > 2)
      df[,paste0(prefix,"_sum")] <- rowSums(df[,-1], na.rm=T)
    return(df)
  }

  q_pos <- grep("^q", names(river_ts)[-1])+1
  if (length(q_pos) == 1)
    river_ts[[q_pos]] <- add_meancol(river_ts[[q_pos]])
  ka_pos <- grep("^ka", names(river_ts)[-1])+1
  if (length(ka_pos) == 1)
    river_ts[[ka_pos]] <- add_meancol(river_ts[[ka_pos]])
  i_pos <- grep("^i", names(river_ts)[-1])+1
  if (length(i_pos) == 1)
    river_ts[[i_pos]] <- add_meancol(river_ts[[i_pos]])
  r_pos <- grep("^r", names(river_ts)[-1])+1
  river_ts[[r_pos]] <- add_meancol(river_ts[[r_pos]])
  return(river_ts)
})

rm(river_data,calc_t)



# step through, forward and backward selection
stepwise <- function (river, pattern)#, q_old, q_new)
  {
  riverdata <- river_data_ts[[river]]
  # prepare variables out of all cominations (given by pattern)
  # variables for interaction get replaced by q_new (remove q_old)
  vars1 <- (riverdata[-1] %>% unroll_physical_data() %>%
              lapply(names) %>% unlist() %>% unique())[-1]
  vars2 <- vars1[stringr::str_detect(vars1, pattern)]

  # prepare formulas
  data <- process_model_riverdata(riverdata, c("log_e.coli", vars1)) %>%
    dplyr::select(-datum)

  # Definition of null and full models
  null <- lm(log_e.coli ~ 1, data = data)
  full <- lm(log_e.coli ~ .^2, data = data)

  # Definition maximum number of steps
  nsteps <- ifelse(round(nrow(data)/10) < 10, round(nrow(data)/10), 10 )
  selection <- list()
  fmla <- list()

  # Creating list of candidate models with 1 ...n predictors
  for(i in 1: nsteps)
  {

    selection[[i]] <- step(null, data = data,
                           direction = "forward",
                           list(lower=null, upper=full), steps = i)
    fmla[[i]] <- as.list(selection[[i]]$call)$formula

  }


  selection
  }


# order of pattern, q_old and q_new is important!
  fb <- stepwise(river = river, pattern = "(i_mean|q_mean|r_mean|ka_mean)")#,
                            #q_old = "q_cochem",
                            #q_new = "q_cochem_abs_1")


  names(fb) <- sprintf(paste0(river,"model_%02d"), seq_along(1:length(fb)))

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

 river_stat_tests <- sapply(fb, get_stat_tests)%>%
    t() %>%
    dplyr::as_tibble(rownames = "model")  %>%

    dplyr::bind_rows(.id = "river") %>%

    dplyr::mutate(stat_correct = N > .05 & BP > .05)

# creating list of independent training rows
train_rows <- caret::createFolds(1:nrow(fb[[paste0(river, "model_01")]]$model),
                                         k = 5, list = T, returnTrain = T)



  names(fb)

  fmla <- list()

for(i in names(fb))
  fmla[[i]] <- as.list(fb[[i]]$call)$formula

test_beta <- function(true, false, percentile)
  { if( pbeta(q = percentile, shape1 = true + 1, shape2 = false + 1) > 0.05)
  {TRUE}
    else{FALSE}
  }


river_stat_tests$in95 <- river_stat_tests$below95 <-
    river_stat_tests$below90 <- river_stat_tests$in50 <- 0


for(i in names(fb))
  for(j in 1:5)
  {
  {
  training <- as.data.frame(fb[[i]]$model)[c(train_rows[[j]]),]
  test <- as.data.frame(fb[[i]]$model)[-c(train_rows[[j]]),]

  fit <- rstanarm::stan_glm(fmla[[i]], data = training)

  df <- apply(rstanarm::posterior_predict(fit, newdata = test), 2, quantile,
        probs = c(0.025, 0.25, 0.75, 0.9, 0.95, 0.975)) %>% t() %>% as.data.frame() %>%
    dplyr::mutate(log_e.coli = test$log_e.coli,
                  below95 = log_e.coli < `95%`,
                  below90 = log_e.coli < `90%`,
                  within95 = log_e.coli < `97.5%`& log_e.coli > `2.5%`,
                  within50 = log_e.coli < `75%`& log_e.coli > `25%`,
  )

  river_stat_tests$in95[river_stat_tests$model == i] <-
    river_stat_tests$in95[river_stat_tests$model == i] +
    test_beta(true = sum(df$within95), false = sum(!df$within95), percentile = .95 )
  river_stat_tests$below95[river_stat_tests$model == i] <-
    river_stat_tests$below95[river_stat_tests$model == i] +
    test_beta(true = sum(df$below95), false = sum(!df$below95), percentile = .95 )
  river_stat_tests$below90[river_stat_tests$model == i] <-
    river_stat_tests$below90[river_stat_tests$model == i] +
    test_beta(true = sum(df$below90), false = sum(!df$below90), percentile = .90 )
  river_stat_tests$in50[river_stat_tests$model == i] <-
    river_stat_tests$in50[river_stat_tests$model == i] +
    test_beta(true = sum(df$within50), false = sum(!df$within50), .5)

 }
}


sorted_modellist <- river_stat_tests %>%
  filter( below95 == 5 & below90 == 5& in50==5) %>%
  dplyr::arrange(desc(R2))

best_valid_model_stats <- sorted_modellist[1,]
best_valid_model <- fb[[best_valid_model_stats$model]]


stanfit <- rstanarm::stan_glm(fmla[[best_valid_model_stats$model]],
                              data = best_valid_model$model)

return(list(sorted_modellist, best_valid_model, stanfit))

}


results
