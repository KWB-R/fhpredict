# provide_input_data -----------------------------------------------------------

#' Provide Input Data for the Model Calibration
#'
#' Provide data in the list format as returned by Carsten Vick's import function
#'
#' @param user_id user id
#' @param spot_id bathing spot id
#' @param require_hygiene logical. If \code{TRUE} (default) the function will
#'   raise an error if there are no hygienic data
#' @export
#'
provide_input_data <- function(user_id, spot_id, require_hygiene = TRUE)
{
  #kwb.utils::assignPackageObjects("fhpredict")
  #user_id=11;spot_id=57

  # Define shortcut to kwb.utils::selectColumns()
  get <- kwb.utils::selectColumns

  # Prepare the result list
  result <- list()

  # Define function returning the name for an element of the result list
  result_element <- function(prefix) sprintf("%s_spot%d", prefix, spot_id)

  # Look for microbiological measurements
  measurements <- api_get_measurements(user_id, spot_id)

  # Add microbiological measurements to the result or return if there are no
  # measurements
  if (nrow(measurements) == 0 && require_hygiene) clean_stop(
    get_text("no_measurements", user_id = user_id, spot_id = spot_id
    ))

  # Create "hygiene" data frame
  result[[result_element("hygiene")]] <- data.frame(
    datum = reset_time(iso_timestamp_to_local_posix(get(measurements, "date"))),
    e.coli = get(measurements, "conc_ec")
  )

  # Look for rain data
  rain <- api_get_rain(user_id, spot_id)

  # Add rain measurements to the result or return if there are no rain data
  if (nrow(rain) == 0) {
    clean_stop(get_text("no_rain_data", user_id = user_id, spot_id = spot_id))
  }

  result[[result_element("r")]] <- data.frame(
    datum = reset_time(get(rain, "dateTime")),
    r_radolan = get(rain, "value")
  )

  # Look for discharge measurements. See tutorial for how to add some discharge
  # data
  discharge <- api_get_discharge(user_id, spot_id)

  # Add discharges to the result if there are any discharges
  if (nrow(discharge)) {
    result[[result_element("q")]] <- data.frame(
      datum = reset_time(get(discharge, "dateTime")),
      q_1 = get(discharge, "value")
    )
  }

  # Look for global irradiation data
  irradiances <- api_get_irradiances(user_id, spot_id)

  # Add irradiances to the result if there are any irradiances
  if (nrow(irradiances)) {
    result[[result_element("i")]] <- data.frame(
      datum = reset_time(get(irradiances, "dateTime")),
      i_1 = get(irradiances, "value")
    )
  }

  # Helper function to add further list entries (or not if length(x) == 0)
  add_series <- function(result, type, prefix, x) {

    x <- collect_series_measurements(type, prefix, user_id, spot_id)

    if (length(x)) {

      result[[result_element(prefix)]] <- kwb.utils::renameColumns(
        merge_series_measurements(x),
        list(dateTime = "datum")
      )
    }

    result
  }

  # Add purification plant measurements if there are any
  result <- add_series(result, type = "plant", prefix = "ka")

  # Add generic input measurements if there are any
  result <- add_series(result, type = "generic", prefix = "gen")

  # Rename the elements in the result data frame?

  # Return the result data structure
  result
}

# collect_series_measurements --------------------------------------------------
collect_series_measurements <- function(type, prefix, user_id, spot_id)
{
  get_functions <- kwb.utils::selectElements(elements = type, x = list(
    plant = list(
      objects = api_get_plant,
      measurements = api_get_plant_measurements
    ),
    generic =list(
      objects = api_get_generic,
      measurements = api_get_generic_measurements
    )
  ))

  # Look for purification plant measurements
  series <- (get_functions$objects)(user_id, spot_id)

  if (kwb.utils::defaultIfNULL(nrow(series), 0) > 0) {

    lapply(
      X = stats::setNames(series$id, sprintf("%s_%d", prefix, series$id)),
      FUN = get_functions$measurements,
      user_id = user_id,
      spot_id = spot_id
    )
  }
}

# merge_series_measurements ----------------------------------------------------
merge_series_measurements <- function(x)
{
  stopifnot(is.list(x))

  merged <- kwb.utils::mergeAll(
    lapply(x, kwb.utils::removeColumns, "id"),
    by = "dateTime",
    all = TRUE
  )

  names(merged) <- gsub("^value\\.", "", names(merged))

  merged
}
