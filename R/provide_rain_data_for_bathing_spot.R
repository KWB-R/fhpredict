# provide_rain_data_for_bathing_spot -------------------------------------------

#' Provide Rain Data for one Bathing Spot
#'
#' The function considers (i) the "catchment" area that is defined for the
#' bathing spot and (ii) the date range that is taken from the range of dates
#' for which measurements are available. Rain data are read from binary Radolan
#' files, cropped around the "catchment" area and and averaged over all layears
#' for each raster cell.
#'
#' @param user_id user id
#' @param spot_id bathing spot id
#' @param sampling_time expected sampling time. Will be used to select the
#'   corresponding Radolan files.
#' @param date_range vector of two Date objects giving the first and last day of
#'   rain data to be loaded. If \code{NULL} (the default) the range of dates is
#'   determined from the range of dates for which water quality measurements are
#'   available.
#' @param comment character string to be written to the field "comment" of the
#'   rain database table.
#' @return vector of integer containing the IDs of the records inserted into the
#'   "rains" database table.
#' @export
#'
provide_rain_data_for_bathing_spot <- function(
  user_id, spot_id, sampling_time = "1050", date_range = NULL,
  comment = paste("imported:", Sys.time())
)
{
  # Get metadata about the current bathing spot
  spot <- api_get_bathingspot(spot_id = spot_id)

  # Provide the polygon in the same structure as returned by
  # select_relevant_rain_area(): a recursive list
  area_list <- convert_area_structure(spot_area = spot$area)

  # If no date range is given, determine the range of dates for which rain data
  # are required from the range of dates for which water quality measurements
  # are available.
  date_range <- kwb.utils::defaultIfNULL(
    date_range,
    get_date_range_of_measurements(user_id, spot_id)
  )

  to_text_range <- function(x) as.character(gsub("-", "", x))

  # Read rain data for the corresponding time period
  system.time(radolan_stack <- read_radolan_raster_stack(
    date_from = to_text_range(date_range[1]),
    date_to = to_text_range(date_range[2]),
    bathing_season_only = TRUE,
    sampling_time = sampling_time
  ))

  # Crop the polygons from each raster layer
  cropped <- crop_area_from_radolan_stack(area_list, radolan_stack)

  # Get the mean over all layers for each point on the raster
  aggregated <- raster::cellStats(cropped, stat = mean)

  # The day information can be restored from the names of the layers
  dates <- as.Date(substr(names(radolan_stack), 2, 9), format = "%Y%m%d")

  # Provide rain data in a data frame
  rain <- data.frame(
    datum = dates,
    rain = as.numeric(aggregated) / 10
  )

  # Add rain data frame to the database
  fhpredict::api_add_rain(
    user_id, spot_id, rain,
    time_string = sampling_time_to_time_string(sampling_time),
    comment = comment
  )
}

# get_date_range_of_measurements -----------------------------------------------
get_date_range_of_measurements <- function(user_id, spot_id)
{
  measurements <- api_measurements_spot(user_id, spot_id)

  timestamps <- kwb.utils::selectColumns(measurements, "date")

  range(as.Date(iso_timestamp_to_local_posix(timestamps)))
}

# sampling_time_to_time_string -------------------------------------------------
sampling_time_to_time_string <- function(sampling_time)
{
  paste0(
    substr(sampling_time, 1, 2),
    ":",
    substr(sampling_time, 3, 4),
    ":00"
  )
}