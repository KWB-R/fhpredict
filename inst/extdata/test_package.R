# Test adding and reading models (see also the vignette) -----------------------
if (FALSE)
{
  #kwb.utils::assignPackageObjects("fhpredict")

  # Get meta information on all models that are stored for one bathing spot of
  # one user
  (model_info <- fhpredict::api_get_model(3, 18))

  # Add something that is not a model
  model_id <- fhpredict:::api_add_model(
    user_id = 3, spot_id = 18, model = cars, comment = "Mein Automodell"
  )

  # Read the fake model back
  my_cars <- fhpredict::api_get_model(3, 18, model_id)

  # Nothing should have changed!
  identical(cars, my_cars)

  # Delete a model
  fhpredict::api_delete_model(3, 18, model_id)

  # Now: Add a real STAN model object to the database
  model <- kwb.flusshygiene.app:::model_kleine_badewiese

  model_id <- fhpredict::api_add_model(
    user_id = 3,
    spot_id = 18,
    model = model,
    comment = paste(
      "Modell fuer die Kleine Badewiese, wie es im Paket ",
      "kwb.flusshygiene.app gespeichert ist."
    )
  )

  # Reading the model back from the database
  model_back <- fhpredict::api_get_model(3, 18, model_id)

  # The models are not identical
  identical(model, model_back)

  # Get the model structures as text
  out_1 <- capture.output(str(model))
  out_2 <- capture.output(str(model_back))

  # Show the differences in the structures
  out_1[out_1 != out_2]
  out_2[out_1 != out_2]

  # The difference is only in two environments. What does that mean?
}

# Do some test related to bathingspots -----------------------------------------
if (FALSE)
{
  # Get all users
  fhpredict::api_get_users()

  # Get main info on all bathingspots
  bathing_spots <- fhpredict::api_get_bathingspot(user_id = 3)
  nrow(bathing_spots)

  all_bathing_spots <- fhpredict::api_get_bathingspot(user_id = 3, limit = 10000)
  nrow(all_bathing_spots)

  #View(all_bathing_spots)

  spot <- fhpredict::api_get_bathingspot(user_id = 3, spot_id = 1441)

  stopifnot(spot$id == 1441)
  spot$nameLong

  # Polygon as GeoJSON string
  jsonlite::toJSON(spot$area)

  # Polygon coordinates as data frame
  spot$area_coordinates

  # It is also possible to get a bathing spot without specifying a user id
  spot_100 <- fhpredict::api_get_bathingspot(spot_id = 100)

  # Get the "flat" data out of the list
  fhpredict:::extract_flat_information(spot_100)

  # What kind of (non-NULL) data do we get?
  str(kwb.utils::excludeNULL(spot_100), 2)

  # Does the list contain information from foreign database tables?
  measurements <- fhpredict:::flatten_recursive_list(spot_100$measurements)

  # Have a look at the measurements
  View(measurements)

  # Is this the same as what the measurement endpoint provides?
  response <- fhpredict::safe_postgres_get("users/3/bathingspots/100/measurements")

  # Convert list structure to a data frame
  measurements2 <- fhpredict:::flatten_recursive_list(response$data)

  # Compare with what the former request returned
  identical(measurements2, measurements)

  # Try to access the purification plant measurements...
  path <- "users/3/bathingspots/18/purificationPlants/1/measurement"

  fhpredict::postgres_get(path)
  fhpredict::postgres_post(path) # -> not yet implemented

  response <- kwb.utils::getAttribute(result, "response")
  str(response)

  str(fhpredict::postgres_get(path)$data)

  # Try to access the generic inputs...
  path <- "users/3/bathingspots/18/genericInputs/1"

  (result <- fhpredict::postgres_post(
    path, body = list(name = "lirum larum 99")
  ))
}

# Test the cache and other things ----------------------------------------------
if (FALSE)
{
  str(river_data, 2)

  # What users are available?
  users <- api_get_users()

  # Get purification plant data
  spots <- get_bathingspots_for_user(user_id = 3)

  result <- fhpredict:::safe_postgres_get("users/3/bathingspots/2430/purificationPlants")

  result <- fhpredict:::safe_postgres_get("users/3/bathingspots/18/purificationPlants/1/measurement")

  # Measurements from bathing spots: ok
  measurements <- get_measurements_at_bathing_spot(user_id = 3, spot_id = 18)

  # Measurements from purification plats: Not yet implemented
  result <- fhpredict::postgres_post(
    path = "users/3/bathingspots/18/purificationPlants/1/measurement",
    body = body_measurement(
      date = "2019-01-31",
      dateTime = "10:00:00",
      value = 123,
      comment = "test data added by Hauke"
    )
  )

  # Generic measurements
  result <- fhpredict:::safe_postgres_get("users/3/bathingspots/18/genericInputs/1")

  result
}

# What different results do I get? ---------------------------------------------
if (FALSE)
{
  #kwb.utils::assignPackageObjects("fhpredict")

  path_1 <- fhpredict:::path_bathingspot()
  path_2 <- fhpredict:::path_bathingspot(user_id = 3)
  path_3 <- fhpredict:::path_bathingspot(user_id = 3, spot_id = 18)

  path_1 <- "bathingspots"
  path_2 <- "users/3/bathingspots"
  path_3 <- "users/3/bathingspots/18"
  path_4 <- "bathingspots/18"

  token <- fhpredict:::get_postgres_api_token()
  config <- httr::add_headers("Authorization" = paste("Bearer", token))

  responses <- lapply(c(path_1, path_2, path_3, path_4), function(path) {

    url <- paste0(assert_final_slash(get_environment_var("API_URL")), path)

    httr::GET(url, config = config)
  })

  str(responses, 2)

  sapply(responses, httr::status_code)

  contents_raw <- lapply(responses, httr::content, as = "raw")
  contents_text <- lapply(responses, httr::content, as = "text")
  contents_parsed <- lapply(responses, httr::content, as = "parsed")

  str(contents_raw, 1)
  str(contents_text, 1)
  str(contents_parsed, 2)

  lapply(contents_text, substr, 1, 100)

  d1 <- contents_parsed[[1]][[1]]
  d2 <- contents_parsed[[2]]$data[[1]]

  # d1 has "region" but no "user"
  d1$region

  # d2 has "user" but no "region"
  d2$user

  out1 <- capture.output(str(d1, 1))
  out2 <- capture.output(str(d2, 1))

  out1[out1 != out2]
  out2[out1 != out2]

  if (type == "GET" && status != 200) {
    error <- kwb.utils::defaultIfNULL(parsed$message, "")
    message(sprintf("GET request '%s' returned with error:\n'%s'", path, error))
    return(structure(list(), response = response))
  }

  if (type == "POST" && status != 201) {
    error <- kwb.utils::defaultIfNULL(parsed$message, "")
    message(sprintf("POST request '%s' returned with error:\n'%s'", path, error))
    return(structure(list(), response = response))
  }

  if (type == "GET") {

    parsed

  } else {

    parsed$data
  }
}
