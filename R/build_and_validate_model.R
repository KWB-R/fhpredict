# build_and_validate_model -----------------------------------------------------

#' @importFrom rlang .data
#' @keywords internal
build_and_validate_model <- function(
  river_data = NULL, river = NULL, spot_data, prefix = "", n_folds = 5,
  dbg = TRUE
)
{
  #kwb.utils::assignPackageObjects("fhpredict")
  #river_data=NULL;river=NULL;prefix="";n_folds=5

  # Check the arguments and stop if anything is not ok
  check_args_build_and_validate(river_data, river, spot_data)

  # Prepare all data frames (select summer season, log-transform rain, add mean,
  # ...) add merge them to one big data frame
  model_data <- provide_data_for_lm(
    riverdata = prepare_river_data(spot_data),
    pattern = "(i_mean|q_mean|r_mean|ka_mean)",
    dbg = dbg
  )

  # Step through, forward and backward selection
  models <- stepwise(kwb.utils::removeColumns(model_data, "datum"))

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

  if (nrow(sorted_models) == 0) {

    message(get_text("could_not_build_model"))
    return(list())
  }

  # Select the best model from models
  best_model <- kwb.utils::selectElements(models, sorted_models$model[1])

  stanfit <- rstanarm::stan_glm(
    formula = get_formula(best_model),
    data = kwb.utils::selectElements(best_model, "model")
  )

  list(
    sorted_models = sorted_models,
    best_model = best_model,
    stanfit = stanfit
  )
}

# check_args_build_and_validate ------------------------------------------------
check_args_build_and_validate <- function(river_data, river, spot_data)
{
  if (! is.null(river_data)) {
    clean_stop(get_text("river_data_not_supported"))
  }

  if (! is.null(river)) {
    clean_stop(get_text("river_not_supported"))
  }

  if (! all(sapply(spot_data, is.data.frame))) {
    clean_stop(get_text("spot_data_expected_type"))
  }
}

# stepwise ---------------------------------------------------------------------
stepwise <- function(model_data)
{
  # Definition of null and full models
  null <- stats::lm(log_e.coli ~ 1, data = model_data)
  full <- stats::lm(log_e.coli ~ .^2, data = model_data)

  # Definition maximum number of steps. 10 at maximum
  max_steps <- min(round(nrow(model_data) / 10), 10)

  if (max_steps == 0) {
    clean_stop(get_text("max_steps_is_zero"))
  }

  # Creating list of candidate models with 1 ... max_steps predictors
  result <- lapply(X = seq_len(max_steps), FUN = function(steps) {
    try(stats::step(
      object = null,
      scope = list(upper = full, lower = null),
      direction = "forward",
      steps = steps
    ))
  })

  failed <- sapply(result, inherits, "try-error")

  if (any(failed)) {

    clean_stop(get_text("step_failed", details = new_line_collapsed(sprintf(
      "step = %d: %s", which(failed), sapply(result[failed], as.character)
    ))))
  }

  result
}

# provide_data_for_lm ----------------------------------------------------------
provide_data_for_lm <- function(riverdata, pattern = "", dbg = TRUE)
{
  unrolled_data <- riverdata %>%
    remove_hygiene_data() %>%
    kwb.flusshygiene::unroll_physical_data()

  # Prepare variables out of all combinations (given by pattern)
  # Variables for interaction get replaced by q_new (remove q_old)
  all_columns <- lapply(unrolled_data, names)

  # Determine variable names: all different column names except "datum"
  variables <- setdiff(unique(unlist(all_columns)), "datum")

  # Filter for variables matching the pattern
  if (nzchar(pattern)) {

    variables <- kwb.utils::catAndRun(
      get_text("filtering_variables", n = length(variables), pattern = pattern),
      expr = grep(pattern, variables, value = TRUE),
      dbg = dbg
    )
  }

  kwb.utils::catIf(dbg, get_text(
    "using_variables",
    n = length(variables),
    varlist = kwb.utils::stringList(variables, collapse = "\n- ")
  ))

  variables <- c("log_e.coli", variables)

  # Prepare formulas
  data <- kwb.flusshygiene::process_model_riverdata(riverdata, variables)

  if (nrow(data) == 0) {
    utils::str(riverdata)
    clean_stop(get_text("process_returned_no_data"))
  }

  data
}

# remove_hygiene_data ----------------------------------------------------------
remove_hygiene_data <- function(datalist)
{
  stopifnot(is_river_data_element(datalist))

  hygiene_element <- grep("^hygiene", names(datalist), value = TRUE)

  result <- kwb.utils::catAndRun(
    get_text("removing_data_frame", element = hygiene_element),
    datalist[setdiff(names(datalist), hygiene_element)]
  )

  result
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
            .data$log_e.coli > .data$`25%`
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
  stats::pbeta(
    q = percentile,
    shape1 = sum(is_true) + 1,
    shape2 = sum(! is_true) + 1
  ) > 0.05
}
