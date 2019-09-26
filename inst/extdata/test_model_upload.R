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

  # Provide a model object to be stored in the database
  model <- kwb.flusshygiene.app:::model_grunewaldturm

  # Get the URL to the uploaded file
  model_url <- upload_model(user_id, spot_id, model)

  # Get metadata on available models (again)
  model_info <- fhpredict::api_get_model(user_id, spot_id)

  # Get the URL to the file that was uploaded last for the given model ID
  model_url_2 <- rev(model_info$rmodelfiles.url[model_info$id == model_id])[1]

  # Check if the URL returned directly is the URL returned when getting info on
  # all models
  stopifnot(identical(model_url, model_url_2))

  # Read the model back from the URL
  model_reloaded <- download_model(model_url)

  # Compare original model with downloaded model
  identical(model, model_reloaded)
  out_1 <- capture.output(str(model))
  out_2 <- capture.output(str(model_reloaded))
  out_1[out_2 != out_1]
  out_2[out_2 != out_1]
}

# download_model ---------------------------------------------------------------
download_model <- function(model_url)
{
  # Set path to local file where to put the downloaded model file
  model_file_downloaded <- file.path(tempdir(), basename(model_url))

  # Download the model to the local file
  download.file(model_url, model_file_downloaded, mode = "wb")

  # Read the model from the downloaded file
  readRDS(model_file_downloaded)
}

# upload_model -----------------------------------------------------------------
upload_model <- function(user_id, spot_id, model)
{
  # Set path to local file where to store the model
  model_file <- file.path(tempdir(), "model.rds")

  # Save the model to a local file
  saveRDS(model, model_file)

  # Set path to upload endpoint
  path <- paste0(fhpredict:::path_models(user_id, spot_id, model_id), "/upload")

  # Upload the model file
  result <- fhpredict::postgres_post(
    path = path,
    body = list(upload = httr::upload_file(model_file)),
    encode = "multipart"
  )

  # Return the URL to the uploaded file
  result$data[[1]]$url
}
