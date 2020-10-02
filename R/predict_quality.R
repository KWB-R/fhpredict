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
#'   day of the time period to be predicted. Default: "today"
#' @param to Date object or date string in format yyyy-mm-dd giving the last day
#'   of the time period to be predicted. Default: "today"
#' @param import logical telling whether to import new rain data or not.
#'   Default: \code{TRUE}.
#' @param return_debug_info logical with default \code{FALSE}. If \code{TRUE}
#'   the prediction is not written to the database. Instead, what would be send
#'   to the database is returned with all relevant variables that were used to
#'   prepare the prediction being set as attributes.
#' @param radolan_time time string ("hhmm") against which to match the RADOLAN
#'   file names to be loaded. By default, the latest available time for the day
#'   given in \code{from} is used (for all days within \code{from} and
#'   \code{to}).
#' @return list with elements \code{data}, \code{success}, \code{message} or (if
#'   \code{return_debug_info = TRUE}) data frame representing the predictions
#'   with attributes \code{spot_data}, \code{riverdata_raw}, \code{riverdata},
#'   \code{newdata_raw}, \code{newdata}, \code{prediction} representing
#'   intermediate variables that were used to prepare the prediction  (see
#'   source code of \code{fhpredict::predict_quality} to understand their
#'   meaning)
#' @export
predict_quality <- function(
  user_id, spot_id, from = Sys.Date(), to = from, import = TRUE,
  return_debug_info = FALSE, radolan_time = NULL
)
{
  #kwb.utils::assignPackageObjects("fhpredict")
  #kwb.utils:::assignArgumentDefaults(predict_quality)
  #user_id=11;spot_id=58;to=from;import=FALSE

  (before_from <- as.Date(from) - 1L)
  (before_to <- as.Date(to) - 1L)

  # Default RADOLAN time string: latest available for the day before the first
  # day to predict
  (radolan_time <- kwb.utils::defaultIfNULL(
    radolan_time, utils::tail(available_radolan_times_of_day(before_from), 1L)
  ))

  # Try to get the model that was added last (if any)
  model <- try(get_last_added_model(user_id, spot_id))

  if (is_error(model)) {
    return(create_failure(model))
  }

  # Provide new data for prediction
  newdata <- try({

    # Determine the days of which to load data and the days to be predicted
    (data_days <- determine_date_range(before_from, before_to))
    (prediction_days <- determine_date_range(from, to))

    # Load new (rain) data required for the prediction
    if (import) {
      import_new_data(
        user_id, spot_id, data_days, radolan_time = radolan_time
      )
    }

    # Collect all data that are available for the given bathing spot
    spot_data <- provide_input_data(user_id, spot_id, require_hygiene = FALSE)

    # Prepare the data (filter for bathing season, log-transform rain)
    riverdata_raw <- prepare_river_data(spot_data)

    # If no data are available for the "to" date, add a fake entry for the "to"
    # date (with all values 0) and reorder the data frame by the date
    riverdata <- lapply(riverdata_raw, add_fake_entry_if_required, to)

    # Calculate daily means just in case there is more than one record per day
    riverdata <- lapply(riverdata, calculate_daily_means)

    #identify_date_duplicates(riverdata_raw)
    stopifnot(all(lengths(identify_date_duplicates(riverdata)) == 0L))

    # Provide data frame with all additional variables, as required by the model
    newdata_raw <- provide_data_for_lm(riverdata, for_model_building = FALSE)

    stopifnot(length(identify_date_duplicates(newdata_raw)) == 0L)

    # Filter for the dates to be predicted
    all_dates <- substr(newdata_raw$datum, 1, 10)

    use_me <- all_dates %in% as.character(prediction_days)

    if (! any(use_me)) clean_stop(
      "No data available for these required days for prediction: ",
      kwb.utils::stringList(as.character(data_days))
    )

    #kwb.utils::removeColumns(newdata[use_me, ], "log_e.coli")

    # Names of variables that are acutally used by the model
    model_vars <- get_indipendent_variables(model$formula)

    # Keep only the rows related to days to be predicted and keep only the
    # variables that are required by the model
    columns <- c("datum", model_vars)

    newdata <- kwb.utils::selectColumns(newdata_raw[use_me, ], columns)
  })

  if (is_error(newdata)) {
    return(create_failure(newdata))
  }

  result <- try({

    # Get a prediction using the model and the new data
    prediction <- rstanarm::posterior_predict(model, newdata = newdata)

    percentiles <- finish_prediction(prediction, newdata)

    if (return_debug_info) {

      return(structure(
        percentiles,
        spot_data = spot_data,
        riverdata_raw = riverdata_raw,
        riverdata = riverdata,
        newdata_raw = newdata_raw,
        newdata = newdata,
        prediction = prediction
      ))
    }

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

# determine_date_range ---------------------------------------------------------
determine_date_range <- function(from = NULL, to = NULL)
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
    "Either 'from' or 'to' must be given to 'determine_date_range'"
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
import_new_data <- function(user_id, spot_id, days, radolan_time = "1050")
{
  stopifnot(inherits(days, "Date"))

  # Extend days to 5-day periods before each day
  dates <- add_days_before(days, n_days_before = 5)

  urls <- get_radolan_urls_for_days(dates = dates, time = radolan_time)

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

# add_fake_entry_if_required ---------------------------------------------------
add_fake_entry_if_required <- function(df, to)
{
  #df <- riverdata_tmp$hygiene_spot58
  to_date <- as.POSIXct(paste0(to, "00:00:00"))

  if (! to_date %in% df$datum) {

    to_record <- do.call(
      what = data.frame,
      args = c(as.list(to_date), as.list(rep(0, ncol(df) - 1L)))
    )

    df <- rbind(df, stats::setNames(to_record, names(df)))

    df <- df[order(df$datum), ]
  }

  df
}
