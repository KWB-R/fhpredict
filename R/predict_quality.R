# predict_quality --------------------------------------------------------------

#' Predict Water Quality for Bathing Spot Using Stored Model
#'
#' If no time period is specified, the function will first check the
#' measurements for missing E.coli values and do the prediction for the days
#' for which an E.coli value of -1 has been uploaded. If all E.coli measurements
#' are valid, the prediction will be done for the current day
#' (\code{Sys.Date()}).
#'
#' @param user_id user ID
#' @param spot_id bathing spot ID
#' @param from Date object or date string in format yyyy-mm-dd giving the first
#'   day of the time period to be predicted. Default: "yesterday"
#' @param to Date object or date string in format yyyy-mm-dd giving the last day
#'   of the time period to be predicted. Default: "tomorrrow"
#' @param import logical telling whether to import new rain data or not.
#'   Default: \code{TRUE}.
#' @return list with elements \code{data}, \code{success}, \code{message}
#' @export
predict_quality <- function(
  user_id, spot_id, from = Sys.Date() - 1L, to = Sys.Date() + 1L, import = TRUE
)
{
  #kwb.utils::assignPackageObjects("fhpredict")
  #user_id=8;spot_id=43;from=Sys.Date()-1L;to=Sys.Date()+1L;import=TRUE
  #user_id=9;spot_id=41

  # Try to get the model that was added last (if any)
  model <- try(get_last_added_model(user_id, spot_id))

  if (is_error(model)) {
    return(create_failure(model))
  }

  # Provide new data for prediction
  newdata <- try({

    # Determine the days to be predicted
    (days_to_predict <- determine_days_to_predict(from, to))

    # Load new data for the dates to predict
    if (import) {
      import_new_data(user_id, spot_id, days_to_predict)
    }

    # Collect all data that are available for the given bathing spot
    spot_data <- provide_input_data(user_id, spot_id)

    # Prepare the data (filter for bathing season, log-transform rain)
    riverdata_raw <- prepare_river_data(spot_data)

    # Calculate daily means just in case there is more than one record per day
    riverdata <- lapply(riverdata_raw, calculate_daily_means)

    #identify_date_duplicates(riverdata_raw)
    stopifnot(all(lengths(identify_date_duplicates(riverdata)) == 0L))

    # Provide data frame with all additional variables, as required by the model
    newdata_raw <- provide_data_for_lm(riverdata, for_model_building = FALSE)

    stopifnot(length(identify_date_duplicates(newdata_raw)) == 0L)

    # Filter for the days to predict!
    all_dates <- substr(newdata_raw$datum, 1, 10)

    use_me <- all_dates %in% as.character(days_to_predict)

    if (! any(use_me)) clean_stop(
      "No data available for these days to be predicted: ",
      kwb.utils::stringList(as.character(days_to_predict))
    )

    #kwb.utils::removeColumns(newdata[use_me, ], "log_e.coli")

    # Names of variables that are acutally used by the model
    model_vars <- get_indipendent_variables(model$formula)

    # Keep only the rows related to days to be predicted and keep only the
    # variables that are required by the model
    kwb.utils::selectColumns(newdata_raw[use_me, ], c("datum", model_vars))
  })

  if (is_error(newdata)) {
    return(create_failure(newdata))
  }

  result <- try({

    # Get a prediction using the model and the new data
    prediction <- rstanarm::posterior_predict(model, newdata = newdata)

    percentiles <- finish_prediction(prediction, newdata)

    # Delete all predictions
    # api_delete_predictions(user_id, spot_id)
    #
    # # Add predictions to the database
    # add_timeseries_to_database(
    #   path = path_predictions(user_id, spot_id),
    #   data = percentiles
    # )

    api_replace_predictions(user_id, spot_id, percentiles)
  })

  if (is_error(result)) {
    return(create_failure(result))
  }

  return(create_result(
    data = result,
    success = TRUE,
    message = get_text(
      "predictions_posted",
      n = length(result),
      n_updated = length(kwb.utils::getAttribute(result, "updated")),
      n_added = length(kwb.utils::getAttribute(result, "added"))
    )
  ))
}

# determine_days_to_predict ----------------------------------------------------
determine_days_to_predict <- function(from = NULL, to = NULL)
{
  #kwb.utils::assignPackageObjects("fhpredict");from=NULL;to=NULL
  from_missing <- is.null(from)
  to_missing <- is.null(to)

  # If only one of "from" and "to" are given, return the one that is given
  if ((from_missing && ! to_missing) || (to_missing && ! from_missing)) {
    return(as.Date(if (from_missing) to else from))
  }

  # If "from" and "to" are given, return a sequence of days between the two
  if (! from_missing && ! to_missing) {
    return(seq(as.Date(from), as.Date(to), by = 1L))
  }

  clean_stop(
    "Either 'from' or 'to' must be given to 'determine_days_to_predict'"
  )
}

# get_days_with_missing_values -------------------------------------------------
get_days_with_missing_values <- function(hygiene)
{
  is_missing <- missing_ecoli(hygiene)

  if (! any(is_missing)) {
    return(NULL)
  }

  times <- kwb.utils::selectColumns(hygiene, "datum")[is_missing]

  as.Date(substr(times, 1, 10))
}

# import_new_data --------------------------------------------------------------
import_new_data <- function(user_id, spot_id, days)
{
  stopifnot(inherits(days, "Date"))

  # Extend days to 5-day periods before each day
  dates <- add_days_before(days, n_days_before = 5)

  urls <- get_radolan_urls_for_days(dates = dates, time = "1050")

  control <- provide_rain_data(user_id, spot_id, urls = urls, info = FALSE)

  while (control$remaining > 0) {
    control <- provide_rain_data(control = control)
  }

  # Import new data from other sources...
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
