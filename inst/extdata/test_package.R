# Test adding and reading models -----------------------------------------------
if (FALSE)
{
  #kwb.utils::assignPackageObjects("fhpredict")

  # Read all models that are stored for one bathing spot of one user
  models <- fhpredict::api_get_model(3, 18)

  # Get meta information on the models
  model_info <- kwb.utils::getAttribute(models, "model_info")

  # Select a model by looking at the comment
  models[[which(model_info$comment == "Again only cars")[1]]]

  # Delete a model
  path <- fhpredict:::path_models(user_id = 3, spot_id = 18, model_id = 28)
  path <- fhpredict:::path_models(user_id = 3, spot_id = 18)
  fhpredict::postgres_delete(path)

  # Provide a model object
  model <- kwb.flusshygiene.app:::model_grunewaldturm

  # Add the model object to the database
  fhpredict::api_add_model(
    user_id = 3, spot_id = 18, model = model,
    comment = paste(
      "Modell fuer die Badestelle Grunewaldturm, wie es im Paket ",
      "kwb.flusshygiene.app gespeichert ist."
    )
  )

  model_id <- fhpredict:::api_add_model(
    user_id = 3, spot_id = 18, model = cars, comment = "Test mode"
  )

  # Read the fake model back
  my_cars <- fhpredict::api_get_model(3, 18, model_id)

  # Nothing should have changed!
  identical(cars, my_cars)

  # Now: Add a real STAN model
  model <- kwb.flusshygiene.app:::model_kleine_badewiese

  # Save a new model in the database
  model_id <- fhpredict::api_add_model(
    user_id = 3, spot_id = 18, model = model,
    comment = "Brandneues Modell fuer die Kleine Badewiese"
  )

  # Reading the model back from the database
  model_back <- fhpredict::api_get_model(
    user_id = 3, spot_id = 18, model_id = model_id
  )

  # The models are not identical
  identical(model, model_back)

  # Get the model structures as text
  out_1 <- capture.output(str(model))
  out_2 <- capture.output(str(model_back))

  # Show the differences in the structures
  out_1[out_1 != out_2]
  out_2[out_1 != out_2]
}

# MAIN -------------------------------------------------------------------------
if (FALSE)
{
  path <- "bathingspots"

  content <- fhpredict::postgres_get(path)

  bathing_spots <- dplyr::bind_rows(lapply(content, function(x) {
    kwb.utils::asNoFactorDataFrame(
      x[! sapply(x, function(xx) is.null(xx) || is.list(xx))]
    )
  }))

  View(bathing_spots)

  bathing_spots2 <- fhpredict:::api_bathingspots(user_id = 3)

  table(bathing_spots2$id %in% bathing_spots$id)
  table(bathing_spots$id %in% bathing_spots2$id)

  result <- fhpredict::postgres_get("bathingspots/10")

  path <- "users/3/bathingspots/18/genericInputs/1"

  (result <- fhpredict::postgres_post(
    path, body = list(name = "lirum larum 99")
  ))

  path <- "users/3/bathingspots/18/purificationPlants/1/measurement"

  fhpredict::postgres_get(path)
  fhpredict::postgres_post(path) # -> not yet implemented
  response <- kwb.utils::getAttribute(result, "response")
  str(response)

  str(fhpredict::postgres_get(path)$data)

  View(bathing_spots)
}

# Test the cache and other things ----------------------------------------------
if (FALSE)
{
  str(river_data, 2)

  clear_cache()

  # What users are available?
  users <- api_users()

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

  token <- get_postgres_api_token()
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
