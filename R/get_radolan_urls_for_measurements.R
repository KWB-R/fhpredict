# get_radolan_urls_for_measurements --------------------------------------------

#' Determine URLs to Radolan Files for Measurements
#'
#' Determine the URLs to the Radolan files that are required to calibrate
#' a model. For each day of measurement files (for the day of measurements and
#' the five days before) are required.
#'
#' @param user_id user id
#' @param spot_id bathing spot id
#' @param sampling_time expected sampling time. Will be used to select the
#'   corresponding Radolan files.
#' @param date_range vector of two Date objects giving the first and last day of
#'   rain data to be loaded. If \code{NULL} (the default) the range of dates is
#'   determined from the range of dates for which water quality measurements are
#'   available.
#' @param all_in_range if \code{TRUE} (the default is \code{FALSE}) rain data
#'   for all days between the first and last day of a measurement are loaded.
#'   By default (\code{all_in_range = FALSE}), data for the days of measurement
#'   and for the days within a \code{n_days_before}-day time period before each
#'   day of measurement, are loaded.
#' @param n_days_before number of days before a measurement for which to load
#'   rain data
#' @return named vector of character containing the URLs to the Radolan files
#'   required for the measurements storted in the database. The names of the
#'   elements are strings in which the date and time is encoded in the format
#'   yyyymmddHHMM, e.g. "201809171050" for "2019-09-18 10:50"
#' @export
#' @examples
#' \dontrun{
#' urls <- get_radolan_urls_for_measurements(user_id = 5, spot_id = 41)
#' }
get_radolan_urls_for_measurements <- function(
  user_id, spot_id, sampling_time = "1050", date_range = NULL,
  all_in_range = FALSE, n_days_before = 5
)
{
  # Get the dates for which E. coli measurements are available
  if (is.null(date_range) || ! all_in_range) {

    dates_all <- get_unique_measurement_dates(user_id, spot_id)

    if (length(dates_all) == 0) {

      clean_stop(get_text(
        "no_measurements", user_id = user_id, spot_id = spot_id
      ))
    }
  }

  # Determine URLs to Radolan files to be downloaded and read
  if (all_in_range) {

    # If no date range is given, determine the range of dates for which rain
    # data are required from the range of dates for which water quality
    # measurements are available.
    if (is.null(date_range)) {
      date_range <- range(dates_all)
    }

    # Helper function to reformat the date from yyyy-mm-dd to yyyymmdd
    to_text_range <- function(x) as.character(gsub("-", "", x))

    return(get_radolan_urls_bucket(
      from = to_text_range(date_range[1]),
      to = to_text_range(date_range[2]),
      time = sampling_time,
      bathing_season_only = TRUE
    ))
  }

  if (is.null(dates_all)) {
    message(get_text("no_measurement_dates"))
    return(character())
  }

  # Reduce to dates within the bathing season
  dates <- dates_all[is_in_bathing_season(dates_all)]

  if (length(dates) == 0) {
    message(get_text("no_measurement_dates_in_season"))
    return(character())
  }

  if (! is.null(date_range)) {
    dates <- dates[kwb.utils::inRange(dates, date_range[1], date_range[2])]
  }

  if (length(dates) == 0) {
    message(get_text(
      "no_measurement_dates_in_range", from = date_range[1], to = date_range[2]
    ))
    return(character())
  }

  # Add up to five days before each date and get URLs to related Radolan files
  get_radolan_urls_for_days(
    dates = add_days_before(dates, n_days_before),
    time = sampling_time
  )
}

# get_unique_measurement_dates -------------------------------------------------

#' Sorted Unique Dates of Measurments
#'
#' @keywords internal
get_unique_measurement_dates <- function(user_id, spot_id)
{
  measurements <- api_get_measurements(user_id, spot_id)

  if (length(measurements) == 0) {
    return(NULL)
  }

  timestamps <- kwb.utils::selectColumns(measurements, "date")

  sort(unique(as.Date(iso_timestamp_to_local_posix(timestamps))))
}
