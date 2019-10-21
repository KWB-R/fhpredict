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

  if (is_error(model)) {
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
    newdata <- provide_data_for_lm(riverdata)#, pattern = "r_mean")
    newdata
  })

  if (is_error(newdata)) {
    return(create_failure(newdata))
  }

  result <- try({

    # Get a prediction using the model and the new data
    prediction <- rstanarm::posterior_predict(model, newdata = newdata)

    percentiles <- finish_prediction(prediction, newdata)

    api_replace_predictions(user_id, spot_id, percentiles)
  })

  if (is_error(result) == 0) {
    return(create_failure(result))
  }

  return(create_result(
    success = TRUE,
    message = get_text("predictions_posted", n = length(result))
  ))
}

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

# finish_prediction ------------------------------------------------------------
finish_prediction <- function(prediction, newdata)
{
  stopifnot(ncol(prediction) == nrow(newdata))

  percentiles <- get_percentiles_from_prediction(prediction)

  percentiles$prediction <- get_quality_from_percentiles(percentiles)

  names(percentiles) <- kwb.utils::multiSubstitute(
    strings = names(percentiles),
    replacements = list("^P" = "percentile", "\\." = "_")
  )

  dates <- kwb.utils::selectColumns(newdata, "datum")

  percentiles$date <- dates
  percentiles$dateTime <- dates

  percentiles
}
