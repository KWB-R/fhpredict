# get_last_added_model ---------------------------------------------------------
get_last_added_model <- function(user_id, spot_id)
{
  # Get available models
  models <- api_get_model(user_id, spot_id)

  if (nrow(models) == 0) {
    clean_stop(get_text(
      "no_models_stored", user_id = user_id, spot_id = spot_id
    ))
  }

  model_id <- kwb.utils::selectElements(models, "id")[nrow(models)]

  api_get_model(user_id, spot_id, model_id)
}

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
      rmodel = get_text("rmodel_deprecated"),
      comment = comment
    )
  )

  # The result should contain exactly one element, namely the model added
  model_data <- kwb.utils::selectElements(result, "data")

  if (length(model_data) != 1) {

    clean_stop(get_text("not_one_model"))
  }

  # Get the ID of the newly created model
  model_id <- kwb.utils::selectElements(model_data[[1]], "id")

  # Now that the model ID is available, upload the model object to a binary file
  model_url <- upload_model(user_id, spot_id, model_id, model)

  message(get_text("model_stored", model_id = model_id))

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
    messageText = get_text("reading_model", what = if (model_id == -1L) {
      "all models"
    } else {
      get_text("model_with_id", model_id = model_id)
    }),
    expr = postgres_get(path = path)
  )

  stop_on_request_failure(result, error_text = if (model_id == -1L) {
    get_text("no_models_stored", user_id = user_id, spot_id = spot_id)
  } else {
    get_text(
      "no_such_model", model_id = model_id, user_id = user_id, spot_id = spot_id
    )
  })

  models <- kwb.utils::selectElements(result, "data")

  # If no model id was given, return the metadata about available models in a
  # data frame
  if (model_id == -1L) {

    model_data <- kwb.utils::removeColumns(
      dframe = flatten_recursive_list(models),
      pattern = "updatedAt|version|evaluation|rmodelfiles"
    )

    return(model_data)
  }

  # There should be exactly one element in the list of models
  stopifnot(length(models) == 1)

  # Get information on the files that have been uploaded for this model
  model_files <- kwb.utils::selectElements(models[[1]], "rmodelfiles")

  # Number of uploaded files
  n_files <- length(model_files)

  if (n_files == 0) {

    clean_stop(get_text("no_model_file", model_id = model_id))
  }

  # Get the URL to the uploaded model file
  model_url <- model_files[[n_files]]$url

  # Read the model file from the URL
  download_model(model_url)
}

# download_model ---------------------------------------------------------------
download_model <- function(model_url)
{
  readRDS(file(model_url, open = "rb"))
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

  result <- safe_postgres_delete(path_models(user_id, spot_id, model_id))

  stopifnot(kwb.utils::selectElements(result, "success"))
  stopifnot(length(kwb.utils::selectElements(result, "data")) == 1)

  message(get_text(
    "model_deleted",
    model_id = model_id,
    comment = kwb.utils::selectElements(result$data[[1]], "comment")
  ))
}
