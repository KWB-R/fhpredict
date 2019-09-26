# api_add_model ----------------------------------------------------------------

#' Add a Model to the Database
#'
#' @param user_id user ID
#' @param spot_id bathing spot ID
#' @param model model object
#' @param comment description of the model. Default: "any comment?"
#' @return This function returns the ID of the added model. The URL to the
#'   uploaded binary file is returned in attribute "model_url". From there,
#'   the model can be read back with \code{\link{readRDS}}.
#' @export
api_add_model <- function(user_id, spot_id, model, comment = "any comment?")
{
  # Send a POST request to the database
  result <- postgres_post(
    path = path_models(user_id, spot_id),
    body = body_model(
      rmodel = "Deprecated. Binary model file has been uploaded.",
               #model_to_text(model),
      comment = comment
    )
  )

  # The result should contain exactly one element, namely the model added
  model_data <- kwb.utils::selectElements(result, "data")

  if (length(model_data) != 1) {
    clean_stop("Not exactly one model was returned as expected!")
  }

  # Get the ID of the newly created model
  model_id <- kwb.utils::selectElements(model_data[[1]], "id")

  # Now that the model ID is available, upload the model object to a binary file
  model_url <- upload_model(user_id, spot_id, model_id, model)

  message(
    "The model has been stored in the database. It has been given the id ",
    model_id, "."
  )

  # Return the model ID
  structure(model_id, model_url = model_url)
}

# upload_model -----------------------------------------------------------------
upload_model <- function(user_id, spot_id, model_id, model)
{
  # Set path to local file where to store the model
  model_file <- file.path(tempdir(), "model.rds")

  # Save the model to a local file
  saveRDS(model, model_file)

  # Upload the model file using the "upload" endpoint
  result <- postgres_post(
    path = paste0(path_models(user_id, spot_id, model_id), "/upload"),
    body = list(upload = httr::upload_file(model_file)),
    encode = "multipart"
  )

  # Return the URL to the uploaded file
  result$data[[1]]$url
}

# api_get_model ----------------------------------------------------------------

#' Read a Model from the Database
#'
#' This function reads the text representation of a model from the database
#' and converts it to a model object. If no model_id is given, all available
#' models for the given user and bathing spot are returned in a list. The
#' metadata about the models (ids, creation dates, comments) are then returned
#' in the attribute "model_info".
#'
#' @param user_id user ID
#' @param spot_id bathing spot ID
#' @param model_id model ID. A value of -1 will return all available models in a
#'   list
#' @return a model object (if \code{model_id} is given and exists in the
#'   database) or a list of model objects if \code{model_id = -1}.
#' @export
api_get_model <- function(user_id, spot_id, model_id = -1L)
{
  #user_id = 3; spot_id = 18; model_id = 19
  #kwb.utils::assignPackageObjects("fhpredict")

  path <- path_models(user_id, spot_id, model_id)

  # Send a GET request to the database
  result <- kwb.utils::catAndRun(
    sprintf(
      "Reading %s from the database",
      if (model_id == -1L) "all models" else paste("model with id =", model_id)
    ),
    expr = postgres_get(path = path)
  )

  stop_on_request_failure(result, sprintf(
    "There is no model with id %d. Call %s for available models.", model_id,
    sprintf("api_get_model(user_id = %d, spot_id = %d)", user_id, spot_id)
  ))

  models <- kwb.utils::selectElements(result, "data")

  # If no model id was given, return the metadata about available models in a
  # data frame
  if (model_id == -1L) {

    return(flatten_recursive_list(models))
  }

  # There should be exactly one element in the list of models
  stopifnot(length(models) == 1)

  # Get information on the files that have been uploaded for this model
  model_files <- kwb.utils::selectElements(models[[1]], "rmodelfiles")

  # Number of uploaded files
  n_files <- length(model_files)

  if (n_files == 0) {

    clean_stop("No model file has yet been uploaded for model_id = ", model_id)
  }

  # Get the URL to the uploaded model file
  model_url <- model_files[[n_files]]$url

  # Read the model file from the URL
  download_model(model_url)

  # Convert the first list element to text
  #text_to_model(text = kwb.utils::selectElements(models[[1]], "rmodel"))
}

# download_model ---------------------------------------------------------------
download_model <- function(model_url)
{
  readRDS(file(model_url, open = "rb"))

  # # Set path to local file where to put the downloaded model file
  # model_file_downloaded <- file.path(tempdir(), basename(model_url))
  #
  # # Download the model to the local file
  # download.file(model_url, model_file_downloaded, mode = "wb")
  #
  # # Read the model from the downloaded file
  # readRDS(model_file_downloaded)
}

# api_delete_model -------------------------------------------------------------

#' Delete a Model from the Database
#'
#' This function deletes a model from the database.
#'
#' @param user_id user ID
#' @param spot_id bathing spot ID
#' @param model_id model ID
#' @export

api_delete_model <- function(user_id, spot_id, model_id)
{
  #kwb.utils::assignPackageObjects("fhpredict")
  stopifnot(all(sapply(list(user_id, spot_id, model_id), is.numeric)))
  stopifnot(all(sapply(list(user_id, spot_id, model_id), `>`, 0)))

  result <- postgres_delete(path_models(user_id, spot_id, model_id))

  stop_on_request_failure(result)

  stopifnot(kwb.utils::selectElements(result, "success"))
  stopifnot(length(kwb.utils::selectElements(result, "data")) == 1)

  message(sprintf(
    "The model with id %d (%s) was deleted.",
    model_id, kwb.utils::selectElements(result$data[[1]], "comment")
  ))
}
