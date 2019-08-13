# api_add_model ----------------------------------------------------------------

#' Add a Model to the Database
#'
#' @param user_id user ID
#' @param spot_id bathing spot ID
#' @param model model object
#' @param comment description of the model. Default: "any comment?"
#' @return This function returns the ID of the added model.
#' @export
api_add_model <- function(user_id, spot_id, model, comment = "any comment?")
{
  # Send a POST request to the database
  result <- postgres_post(
    path = path_models(user_id, spot_id),
    body = body_model(
      rmodel = model_to_text(model),
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

  message(
    "The model has been stored in the database. It has been given the id ",
    model_id, "."
  )

  # Return the model ID
  model_id
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

  path <- path_models(user_id, spot_id, model_id)

  # Send a GET request to the database
  kwb.utils::catAndRun(
    sprintf(
      "Reading %s from the database",
      if (model_id == -1L) "all models" else paste("model with id =", model_id)
    ),
    expr = {
      result <- postgres_get(path = path)
    }
  )

  stop_on_request_failure(result)

  models <- kwb.utils::selectElements(result, "data")

  model_info <- kwb.utils::safeRowBindAll(lapply(models, function(x) {
    x <- kwb.utils::excludeNULL(x, dbg = FALSE)
    kwb.utils::asNoFactorDataFrame(x[setdiff(names(x), "rmodel")])
  }))

  # Return all models in a list
  if (model_id == -1L) {

    result <- lapply(models, function(model_record) {
      text_to_model(text = kwb.utils::selectElements(model_record, "rmodel"))
    })

    return(structure(result, model_info = model_info))
  }

  # Return only the requested model
  index <- which(kwb.utils::selectColumns(model_info, "id") == model_id)

  if (length(index) == 0) {
    print(model_info)
    stop("There is no model with id ", model_id,
         ". See above for available models.")
  }

  # Read the model back from the database
  text_to_model(text = kwb.utils::selectElements(models[[index]], "rmodel"))
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
