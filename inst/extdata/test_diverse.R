# Test 1 -----------------------------------------------------------------------
if (FALSE)
{
  #library(magrittr)
  #library(tidyverse)
  #library(kwb.flusshygiene)

  # Laden von Testdaten
  rivers <- c("havel", "ilz", "isar", "mosel", "rhein", "ruhr", "spree")
  river_paths <- kwb.flusshygiene::get_paths()[paste0(rivers, "data")]

  river_data <- lapply(river_paths, kwb.flusshygiene::import_riverdata)
  names(river_data) <- rivers

  kwb.fakin:::store(river_data, "test_diverse.R")

  river <- "havel"

  # Create model with the function in fhpredict
  set.seed(1)
  result2 <- fhpredict:::build_and_validate_model(
    spot_data = river_data$havel
    #, prefix = "havel"
  )
  #store(result2)

  # Compare
  diffobj::diffStr(restore("result2"), result2)

  # Create model with the original function by Wolfgang Seis
  if (exists("build_and_validate_model")) {

    set.seed(1)
    result1 <- build_and_validate_model(river_data, river)
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
  #### Laden von Testdaten ###################
  rivers <- c("havel")
  river_paths <- kwb.flusshygiene::get_paths()[paste0(rivers, "data")]
  river_data <- lapply(river_paths, kwb.flusshygiene::import_riverdata)
  river <- "havel"
  names(river_data) <- rivers

  fhpredict:::build_and_validate_model

  #### Anwenden der Hauptfunktion ###################
  river_data_ts <- lapply(river_data, fhpredict:::prepare_river_data)

  identical(river_data_ts, restore("river_data_ts"))

  set.seed(1)

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
