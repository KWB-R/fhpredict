#
# Source the whole script first to load the functions defined below
#

# Laden von Testdaten ----------------------------------------------------------
if (FALSE)
{
  rivers <- c("havel", "ilz", "isar", "mosel", "rhein", "ruhr", "spree")
  river_paths <- kwb.flusshygiene::get_paths()[paste0(rivers, "data")]
  river_data <- lapply(river_paths, kwb.flusshygiene::import_riverdata)
  names(river_data) <- rivers

  store(river_data)
}

# Check data structure for compliance with naming convention -------------------
if (FALSE)
{
  river_data <- restore("river_data")

  # river_data is a list
  stopifnot(is.list(river_data))

  # All elements are "river data elements"
  sapply(river_data, fhpredict:::is_river_data_element)

  #x = river_data$havel
  #names(x)[4] <- "ii_me"
  fhpredict:::is_river_data_element(x)
}

# Test prediction --------------------------------------------------------------
if (FALSE)
{
  user_id <- 3
  spot_id <- 42

  date_range <- as.Date(c("2019-10-01", "2019-10-15"))
  dates <- seq(date_range[1], date_range[2], by = 1)

  urls <- fhpredict:::get_radolan_urls_for_days(
    dates = fhpredict:::add_days_before(dates, n_days_before = 5),
    time = "1050"
  )

  control <- fhpredict::provide_rain_data(user_id, spot_id, urls = urls)

  while (control$remaining > 0) {
    control <- provide_rain_data(control = control)
  }

  spot_data <- fhpredict:::provide_input_data(user_id, spot_id)

  riverdata <- fhpredict:::prepare_river_data(spot_data)

  #pattern <- "(i_mean|q_mean|r_mean|ka_mean)"
  pattern <- "r_mean"

  newdata <- fhpredict:::provide_data_for_lm(riverdata, pattern)

  set.seed(1)
  model_info <- fhpredict:::build_and_validate_model(
    spot_data = spot_data[! grepl("^q", names(spot_data))] #river_data$havel[c(1, 4)]
  )

  fhpredict::build_model(user_id, spot_id)


  model <- fhpredict:::api_get_model(user_id, spot_id)

  model <- model_info$stanfit

  (formula <- capture.output(print(model_info$stanfit$formula))[1])

  prediction <- rstanarm::posterior_predict(model, newdata = newdata)

  percentiles <- fhpredict:::get_percentiles_from_prediction(prediction)

  percentiles$prediction <- fhpredict:::get_quality_from_percentiles(percentiles)

  names(percentiles) <- kwb.utils::multiSubstitute(names(percentiles), list(
    "^P" = "percentile", "\\." = "_"
  ))

  percentiles$date <- seq(dates[1], by = 1, length.out = nrow(percentiles))

  fhpredict:::api_replace_predictions(user_id, spot_id, percentiles)

  path <- fhpredict:::path_predictions(user_id, spot_id)
  fhpredict:::api_get_timeseries(path)

  #fhpredict:::add_timeseries_to_database(path, data = percentiles)
  #fhpredict:::api_delete_timeseries(user_id, spot_id, fhpredict:::path_predictions)
}

# Test 1 -----------------------------------------------------------------------
if (FALSE)
{
  #remotes::install_github("kwb-r/fhpredict@v0.5.0")
  #remotes::install_github("kwb-r/fhpredict@dev")

  # Have a look at the definitions
  fhpredict:::build_and_validate_model
  fhpredict:::prepare_river_data

  prepared <- lapply(river_data[names(river_data) != "spree"], fhpredict:::prepare_river_data)

  #store(prepared)
  prepared_orig <- restore("prepared")

  identical(prepared, prepared_orig)

  dfs1 <- prepared_orig$ilz
  dfs2 <- prepared$ilz

  sapply(names(dfs1), function(name) identical(dfs1[[name]], dfs2[[name]]))

  kwb.test::testColumnwiseIdentity(a = dfs1$r_ilz, b = dfs2$r_ilz)

  df <- dfs1$r_ilz
  df <- dfs2$r_ilz

  value_matrix <- as.matrix(df[, -c(1, ncol(df))])
  head(value_matrix)

  range(rowMeans(value_matrix, na.rm = TRUE) - df$r_mean)

  identical(x, y)

  # Create model with the function in fhpredict
  set.seed(1); results <- lapply(river_data, function(x) {
    try(fhpredict:::build_and_validate_model(spot_data = x))
  })

  #store(results)
  results_orig <- restore("results")

  diffobj::diffStr(results, results_orig)
  diffobj::diffStr(results$havel, results_orig$havel)
  diffobj::diffStr(results$ilz, results_orig$ilz)

  diffobj::diffStr(
    results$isar$best_model$model,
    results_orig$isar$best_model$model
  )

  spot_data <- river_data$havel

  sapply(results, inherits, "try-error")

  lapply(results, class)

  # Store the model in the database
  model <- kwb.utils::selectElements(result2, "stanfit")
  fhpredict::api_add_model(user_id, spot_id, model)

  # Get an overview on available models
  available_models <- fhpredict::api_get_model(user_id, spot_id)

  # Reread the last added model
  model_id <- max(available_models$id)
  model_reread <- fhpredict::api_get_model(user_id, spot_id, model_id)

  # Compare model with reread model
  diffobj::diffStr(model, model_reread)

  # Compare
  diffobj::diffStr(restore("result2"), result2)

  # Create model with the original function by Wolfgang Seis
  if (exists("build_and_validate_model")) {

    set.seed(1)
    result1 <- build_and_validate_model(river_data, river = "havel")
    store(result1)

    # Compare original result with result from calling the function in fhpredict
    diffobj::diffStr(result1, result2)
  }

}

# Feed the database with our data from CSV -------------------------------------
if (FALSE)
{
  data_frames <- river_data$havel

  # 1. Hygiene (hygiene_havel)
  # 2. Purification Plant Measurements (ka_havel)
  # 3. Flows (q_havel)
  # 4. Rain (r_havel)

  user_id <- 3; spot_id <- 42

  # 1 Hygiene
  (measurements <- fhpredict:::api_get_measurements(user_id, spot_id))

  # 1a clear
  fhpredict::api_delete_measurements(user_id, spot_id)

  # 1b import
  data <- kwb.utils::renameColumns(data_frames$hygiene_havel, list(
    datum = "date",
    "e.coli" = "conc_ec"
  ))

  data <- data[! duplicated(data$date), ]

  fhpredict:::add_timeseries_to_database(
    path = fhpredict:::path_measurements(user_id, spot_id),
    data = data[1:10, ]
  )

  # 1c check
  (measurements <- fhpredict:::api_get_measurements(user_id, spot_id))

  # 2 Purification plant measurements: Endpoint does not work...

  # 3 Flows
  fhpredict::api_get_discharge(user_id, spot_id)

  # 3a clear
  fhpredict::api_delete_discharge(user_id, spot_id)

  # 3b import
  df <- data_frames$q_havel
  data <- kwb.utils::noFactorDataFrame(
    date = format(df$datum, "%Y-%m-%d"),
    dateTime = format(df$datum, "%H:%M:%S"),
    value = df[[2]]
  )
  str(data)
  path <- fhpredict:::path_discharges(user_id, spot_id)
  fhpredict:::add_timeseries_to_database(path, data = tail(data))

  # 3c check
  fhpredict::api_get_discharge(user_id, spot_id)

  # 4 rain
  fhpredict::api_get_rain(user_id, spot_id)

  # 4a clear
  fhpredict::api_delete_rain(user_id, spot_id)

  # Create a model from the data available in the database
  result <- fhpredict::build_model(user_id, spot_id)
}

# Test 2 -----------------------------------------------------------------------
if (FALSE)
{
  # Get data in the format returned by Carsten Vick's import function
  river_data <- kwb.fakin:::restore("river_data")

  # Have a look at the structure
  lapply(river_data[[1]], head)

  # Define user id and bathing spot id
  user_id <- 3
  spot_id <- 42 #1441

  # Which bathing spots are available?
  print(fhpredict::api_get_users())
  print(fhpredict::api_get_bathingspot(user_id, spot_id))

  # For what days are measurements available?
  fhpredict:::get_unique_measurement_dates(user_id, spot_id)

  # Provide rain data
  fhpredict:::provide_rain_data_for_bathing_spot(user_id, spot_id)

  # Check if the rain data arrived in the database
  fhpredict::api_get_rain(user_id, spot_id)

  # Load input data from the Postgres database
  db_data <- fhpredict::provide_input_data(user_id, spot_id)

  # Have a look at the structure
  lapply(db_data, head)

  # See tutorial for how to add some discharge data

  # Data from purification plants: does not yet work!
  path <- paste0(fhpredict:::path_bathingspot(user_id, spot_id), "/purificationPlants")

  # Create two purification plants
  #fhpredict:::safe_postgres_post(path, body = list(name = "testplant 1"))
  #fhpredict:::safe_postgres_post(path, body = list(name = "testplant 2"))

  # Check if the purification plants have been arrived in the database
  fhpredict:::flatten_recursive_list(fhpredict:::safe_postgres_get(path)$data)

  # Look for measurements at the purification plants
  path_measurements <- paste0(path, "/1/measurements")

  # Read measurements from the database
  fhpredict:::postgres_get(path_measurements)

  # Try to add measurements... (DOES NOT WORK YET!!!)

  fhpredict:::postgres_post(path_measurements)
  fhpredict:::postgres_post(path_measurements, body = list())
  fhpredict:::postgres_post(path_measurements, body = list(
    date = "2019-01-31",
    dateTime = "10:00:00",
    value = 123,
    comment = "This is an example 1"
  ))
}

# Try to add more than one record ----------------------------------------------
if (FALSE)
{
  user_id <- 3; spot_id <- 42

  # What rain data are available?
  fhpredict:::api_get_rain(user_id, spot_id)

  # Delete all rain data
  fhpredict::api_delete_rain(user_id, spot_id)

  # There should be no more rain data right now
  fhpredict:::api_get_rain(user_id, spot_id)

  # Define some fake rain data
  rain <- kwb.utils::noFactorDataFrame(
    datum = c("2019-10-10", "2019-10-11"),
    rain = c(1.23, 2.34)
  )

  # Add rain data, all records at once
  system.time(fhpredict::api_add_rain(
    user_id, spot_id, rain, time_string = "12:13:14"
  ))

  # Read the result
  fhpredict:::api_get_rain(user_id, spot_id)
}

# Test 3 -----------------------------------------------------------------------
if (FALSE)
{
  #### Prerequsite: river_data (loaded on top of this script)

  fhpredict:::build_and_validate_model

  #### Anwenden der Hauptfunktion ###################
  river_data_ts <- lapply(river_data, fhpredict:::prepare_river_data)

  identical(river_data_ts, restore("river_data_ts"))

  set.seed(1)

  river <- "havel"

  system.time(fb <- fhpredict:::get_forward_backward(river_data_ts, river))

  fb2 <- restore("fb")

  compare_str(fb, fb2)

  stat_tests <- fhpredict:::init_stat_tests_data(fb)

  stat_tests2 <- restore("river_stat_tests")

  identical(stat_tests, stat_tests2)

  train_rows <- fhpredict:::get_training_rows(
    model_object = kwb.utils::selectElements(fb, paste0(river, "model_01")),
    n_folds = 5
  )

  identical(train_rows, restore("train_rows"))

  `%>%` <- magrittr::`%>%`
  #stat_tests <- restore("river_stat_tests")
  #train_rows <- restore("train_rows")
  #fb <- restore("fb")

  set.seed(1)

  stat_tests <- fhpredict:::update_stat_tests(stat_tests, fb, train_rows)

  identical(stat_tests, (y <- restore("river_stat_tests_filled")))

  sorted_models <- stat_tests %>%
    dplyr::filter(.data$below95 == 5 & .data$below90 == 5 & .data$in50 == 5) %>%
    dplyr::arrange(dplyr::desc(.data$R2))

  identical(sorted_models, (y <- restore("sorted_modellist")))

  # Select the best model from fb
  best_model <- kwb.utils::selectElements(fb, sorted_models$model[1])

  stanfit <- rstanarm::stan_glm(
    formula = fhpredict:::get_formula(best_model),
    data = kwb.utils::selectElements(best_model, "model")
  )

  identical(stanfit, (y <- restore("stanfit")))

  y

  list(sorted_models, best_model, stanfit)
}

# Functions --------------------------------------------------------------------
store <- function(x) {
  name <- deparse(substitute(x))
  file_name <- sprintf("store_%s.RData", name)
  file <- file.path(kwb.utils::desktop(), "tmp", file_name)
  kwb.utils::catAndRun(
    sprintf("Saving object '%s' to '%s'", name, file_name),
    save(x, file = file)
  )
}

restore <- function(name) {
  file_name <- sprintf("store_%s.RData", name)
  file <- file.path(kwb.utils::desktop(), "tmp", file_name)
  kwb.utils::catAndRun(
    sprintf("Restoring object '%s'", name),
    kwb.utils::loadObject(file, "x")
  )
}

remove_call <- function(x) {
  x[setdiff(names(x), "call")]
}

order_elements <- function(x) {
  x[sort(names(x))]
}

compare_str <- function(x, y) {
  str_x <- capture.output(str(x))
  str_y <- capture.output(str(y))
  stopifnot(length(x) == length(y))
  differs <- str_x != str_y
  kwb.utils::printIf(TRUE, str_x[differs])
  kwb.utils::printIf(TRUE, str_y[differs])
}
