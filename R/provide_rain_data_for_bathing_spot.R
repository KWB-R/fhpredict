# provide_rain_data_for_bathing_spot -------------------------------------------

#' Provide Rain Data for one Bathing Spot
#'
#' The function considers (i) the "catchment" area that is defined for the
#' bathing spot and (ii) the date range that is taken from the range of dates
#' for which measurements are available. Rain data are read from binary Radolan
#' files, cropped around the "catchment" area and and averaged over all layears
#' for each raster cell. The resulting rain data are then stored in the Postgres
#' database. IMPORTANT: Existing rain data will be overwritten in the database!
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
#' @param all_in_range if \code{TRUE} (the default is \code{FALSE}) rain data
#'   for all days between the first and last day of a measurement are loaded.
#'   By default (\code{all_in_range = FALSE}), data for the days of measurement
#'   and for the days within a 5-day time period before each day of measurement,
#'   are loaded.
#' @return vector of integer containing the IDs of the records inserted into the
#'   "rains" database table.
#' @export
#'
provide_rain_data_for_bathing_spot <- function(
  user_id, spot_id, sampling_time = "1050", date_range = NULL,
  comment = paste("imported:", Sys.time()), all_in_range = FALSE
)
{
  #kwb.utils::assignPackageObjects("fhpredict")

  # Determine the URLs to the Radolan files that are required to calibrate
  # a model. For each day of measurement six files (one for the day of
  # measurements and five for the five days before) are required.
  urls <- get_radolan_urls_for_measurements(
    user_id = user_id,
    spot_id = spot_id,
    sampling_time = sampling_time,
    date_range = date_range,
    all_in_range = all_in_range,
    n_days_before = 5
  )

  if (length(urls) == 0) {
    return(NULL)
  }

  # Get metadata about the current bathing spot. Convert the area list structure
  # to a matrix with columns "lon" and "lat". Convert area structure given in
  # coordinate reference system "crs_from" to polygons given in coordinate
  # reference system "crs_to"
  polygon <- coordinates_to_polygon(
    lonlat = get_area_coordinates(api_get_bathingspot(spot_id = spot_id)),
    crs_from = sp::CRS('+proj=longlat +datum=WGS84'),
    crs_to = kwb.dwd:::get_radolan_projection_string()
  )

  # For each URL, read the file and crop the polygon
  list_of_cropped <- lapply(seq_along(urls), function(i) {

    message(sprintf(
      "Reading and cropping from %s (%d/%d)...",
      basename(urls[i]), i, length(urls)
    ))

    # Read the Radolan file and crop the polygon area
    radolan <- kwb.dwd::read_binary_radolan_file(urls[i])

    # Crop the polygon area
    raster::crop(x = radolan, polygon)
  })

  # Stack the cropped areas
  cropped <- raster::stack(list_of_cropped)

  # Get the mean over all layers for each point on the raster
  aggregated <- raster::cellStats(cropped, stat = mean)

  # The day information can be restored from the names of the layers
  dates <- as.Date(substr(names(urls), 1, 8), format = "%Y%m%d")

  # Provide rain data in a data frame
  rain <- data.frame(
    datum = dates,
    rain = as.numeric(aggregated) / 10
  )

  # Clear existing rain from the database
  api_delete_rain(user_id, spot_id)

  # Add rain data frame to the database
  api_add_rain(
    user_id, spot_id, rain,
    time_string = sampling_time_to_time_string(sampling_time),
    comment = comment
  )
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
