# build_and_validate_model -----------------------------------------------------

#' @importFrom rlang .data
#' @keywords internal
build_and_validate_model <- function(
  river_data = NULL, river = NULL, spot_data, prefix = "", n_folds = 5
)
{
  # Check the arguments and stop if anything is not ok
  check_args_build_and_validate(river_data, river, spot_data)

  # Prepare all data frames, among others by calling calc_t()
  riverdata <- prepare_river_data(spot_data)

  # Step through, forward and backward selection
  models <- stepwise(riverdata, pattern = "(i_mean|q_mean|r_mean|ka_mean)")

  # Number the models
  names(models) <- sprintf("%smodel_%02d", prefix, seq_along(models))

  # Remove duplicates in the list of models (may occur if forward selection
  # stops before n steps are gone)
  models <- models[seq_along(unique(models))]

  ### Validation ###

  # Test for classical statistical model assumptions, normality of residuals
  # and heteroskelasdicity
  stat_tests <- init_stat_tests_data(models)

  # Create list of independent training rows
  train_rows <- get_training_rows(model_object = models[[1]], n_folds = n_folds)

  # Fill the test columns in data frame "stat_tests"
  stat_tests <- update_stat_tests(stat_tests, models, train_rows)

  sorted_models <- stat_tests %>%
    dplyr::filter(.data$below95 == 5 & .data$below90 == 5 & .data$in50 == 5) %>%
    dplyr::arrange(dplyr::desc(.data$R2))

  # Select the best model from models
  best_model <- kwb.utils::selectElements(models, sorted_models$model[1])

  stanfit <- rstanarm::stan_glm(
    formula = get_formula(best_model),
    data = kwb.utils::selectElements(best_model, "model")
  )

  list(sorted_models, best_model, stanfit)
}

# check_args_build_and_validate ------------------------------------------------
check_args_build_and_validate <- function(river_data, river, spot_data)
{
  if (! is.null(river_data)) {
    clean_stop(
      "The argument 'river_data' is not supported any more. Please use the ",
      "new argument 'spot_data' to pass a list of data frames related to ONE ",
      "bathing spot ONLY."
    )
  }

  if (! is.null(river)) {
    clean_stop(
      "The argument 'river' is not supported any more. You may use the ",
      "new argument 'prefix' to prefix the names of the returned models."
    )
  }

  if (! all(sapply(spot_data, is.data.frame))) {
    clean_stop(
      "spot_data is expected to be a list of data frames!"
    )
  }
}

# stepwise ---------------------------------------------------------------------
stepwise <- function (riverdata, pattern = "", dbg = TRUE)
{
  # prepare variables out of all cominations (given by pattern)
  # variables for interaction get replaced by q_new (remove q_old)
  variables <- (
    riverdata[-1] %>%
      kwb.flusshygiene::unroll_physical_data() %>%
      lapply(names) %>%
      unlist() %>%
      unique()
  )[-1]

  if (nzchar(pattern)) {

    variables <- kwb.utils::catAndRun(
      dbg = dbg,
      messageText = sprintf(
        "Filtering %d variables for those matching '%s'",
        length(variables), pattern
      ),
      expr = grep(pattern, variables, value = TRUE)
    )
  }

  kwb.utils::catIf(dbg, sprintf(
    "Using %d variables:\n -%s",
    length(variables), kwb.utils::stringList(variables, collapse = "\n- ")
  ))

  # prepare formulas
  data <- kwb.flusshygiene::process_model_riverdata(
    riverdata, variables = c("log_e.coli", variables)
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
      steps = steps
    )
  })
}

# init_stat_tests_data ---------------------------------------------------------
#' @importFrom rlang .data
#' @keywords internal
init_stat_tests_data <- function(models)
{
  sapply(models, get_stat_tests) %>%
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
  stat_tests, models, train_rows, probs = c(0.025, 0.25, 0.75, 0.9, 0.95, 0.975)
)
{
  for (model_name in names(models)) {

    model <- kwb.utils::selectElements(models, model_name)
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
