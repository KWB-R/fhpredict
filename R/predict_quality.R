# predict_quality --------------------------------------------------------------

#' Predict Water Quality for Bathing Spot Using Stored Model
#'
#' @param user_id user ID
#' @param spot_id bathing spot ID
#' @param date Date object or date string in format yyyy-mm-dd
#' @return list with elements \code{data}, \code{success}, \code{message}
#' @export
predict_quality <- function(user_id, spot_id, date = Sys.Date())
{
  #kwb.utils::assignPackageObjects("fhpredict")
  #user_id=3;spot_id=42
  #user_id=5;spot_id=41

  # Try to get the model that was added last (if any)
  model <- try(get_last_added_model(user_id, spot_id))

  if (inherits(model, "try-error")) {

    return(create_failure(model))
  }

  # Provide new data for prediction
  newdata <- try({

    # Collect all data that are available for the given bathing spot
    spot_data <- provide_input_data(user_id, spot_id)

    # Prepare the data (filter for bathing season, log-transform rain)
    riverdata <- prepare_river_data(spot_data)

    # Use only rain data because currently only rain data will be updated
    # automatically!
    provide_data_for_lm(riverdata, pattern = "r_mean")
  })

  if (inherits(newdata, "try-error")) {

    return(create_failure(newdata))
  }

  # Get a prediction using the model and the new data


  path <- path_predictions(user_id, spot_id)

  #str(postgres_get(path)$data)

  result <- postgres_post(path, body = list(
    date = date,
    prediction = "gut"
  ))

  if (length(result) == 0) {

    response <- kwb.utils::getAttribute(result, "response")

    return(create_result(
      success = FALSE,
      message = httr::content(response)$error$message
    ))
  }

  return(create_result(
    success = result$success,
    message = result$message
  ))
}

# get_last_added_model ---------------------------------------------------------
get_last_added_model <- function(user_id, spot_id)
{
  # Get available models
  models <- api_get_model(user_id, spot_id)

  if (nrow(models) == 0) {

    clean_stop(sprintf(
      "No models stored for user_id = %d, spot_id = %d", user_id, spot_id
    ))
  }

  model_id <- kwb.utils::selectElements(models, "id")[nrow(models)]

  api_get_model(user_id, spot_id, model_id)
}
