# provide_input_data -----------------------------------------------------------

#' Provide Input Data for the Model Calibration
#'
#' Provide data in the list format as returned by Carsten Vick's import function
#'
#' @param user_id user id
#' @param spot_id bathing spot id
#' @export
#'
provide_input_data <- function(user_id, spot_id)
{
  # Define shortcut to kwb.utils::selectColumns()
  get <- kwb.utils::selectColumns

  # Prepare the result data structure
  result <- list()

  # Define the end of possible error messages
  no_data_suffix <- sprintf(
    "available for user_id = %d, spot_id = %d.", user_id, spot_id
  )

  # Look for microbiological measurements
  measurements <- api_measurements_spot(user_id, spot_id)

  # Add microbiological measurements to the result or return if there are no
  # measurements
  if (nrow(measurements)) {

    result$hygiene <- data.frame(
      datum = reset_time(
        iso_timestamp_to_local_posix(get(measurements, "date"))
      ),
      e.coli = get(measurements, "conc_ec")
    )

  } else {

    clean_stop("No microbiological measurements ", no_data_suffix)
  }

  # Look for rain data
  rain <- api_get_rain(user_id, spot_id)

  # Add rain measurements to the result or return if there are no rain data
  if (nrow(rain)) {

    result$r <- data.frame(
      datum = reset_time(get(rain, "dateTime")),
      r_radolan = get(rain, "value")
    )

  } else {

    clean_stop("No rain data ", no_data_suffix)
  }

  # Look for discharge measurements. See tutorial for how to add some discharge
  # data
  discharge <- api_get_discharge(user_id, spot_id)

  # Add discharges to the result if there are any discharges
  if (nrow(discharge)) {
    result$q <- data.frame(
      datum = reset_time(get(discharge, "dateTime")),
      discharge = get(discharge, "value")
    )
  }

  # Rename the elements in the result data frame?

  # Return the result data structure
  result
}

# reset_time -------------------------------------------------------------------
reset_time <- function(x)
{
  stopifnot(inherits(x, "POSIXct"))

  as.POSIXct(substr(as.character(x), 1, 10), tz = attr(x, "tzone"))
}
