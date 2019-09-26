# MAIN -------------------------------------------------------------------------
if (FALSE)
{
  # Set a user ID
  user_id <- 3

  # Set a bathing spot ID
  spot_id <- 18

  # Get metadata on available models
  model_info <- fhpredict::api_get_model(user_id, spot_id)

  # Set a model ID
  model_id <- 3 # max(model_info$id)

  fhpredict::api_get_model(user_id, spot_id, model_id)

  # Provide a model object to be stored in the database
  model <- kwb.flusshygiene.app:::model_grunewaldturm

  # Get the URL to the uploaded file
  model_url <- fhpredict:::upload_model(user_id, spot_id, model)

  # Get metadata on available models (again)
  model_info <- fhpredict::api_get_model(user_id, spot_id)

  # Get the URL to the file that was uploaded last for the given model ID
  model_url_2 <- rev(model_info$rmodelfiles.url[model_info$id == model_id])[1]

  # Check if the URL returned directly is the URL returned when getting info on
  # all models
  stopifnot(identical(model_url, model_url_2))

  # Read the model back from the URL
  model_reloaded <- fhpredict:::download_model(model_url)

  # Compare original model with downloaded model
  identical(model, model_reloaded)
  out_1 <- capture.output(str(model))
  out_2 <- capture.output(str(model_reloaded))
  out_1[out_2 != out_1]
  out_2[out_2 != out_1]
}
