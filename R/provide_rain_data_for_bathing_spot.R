# provide_rain_data_for_bathing_spot -------------------------------------------

#' Provide Rain Data for one Bathing Spot
#'
#' Consider to use \code{\link{provide_rain_data}}. For a description see there.
#'
#' @param user_id user id
#' @param spot_id bathing spot id
#' @param sampling_time expected sampling time. Will be used to select the
#'   corresponding Radolan files.
#' @param date_range vector of two Date objects giving the first and last day of
#'   rain data to be loaded. If \code{NULL} (the default) the range of dates is
#'   determined from the range of dates for which water quality measurements are
#'   available.
#' @return list with elements \code{data}, \code{success}, \code{message}. The
#'   \code{data} element contains a vector of IDs identifying the records in
#'   the rain data table associated with the bathing spot given by
#'   \code{user_id} and \code{spot_id}
#' @export
#' @examples
#' \dontrun{
#' provide_rain_data_for_bathing_spot(user_id = 5, spot_id = 41)
#' }
provide_rain_data_for_bathing_spot <- function(
  user_id, spot_id, sampling_time = "1050", date_range = NULL
)
{
  #kwb.utils::assignPackageObjects("fhpredict")
  #user_id=4;spot_id=17;sampling_time="1050";date_range=NULL

  # kwb.utils::warningDeprecated(
  #   old_name = "provide_rain_data_for_bathing_spot",
  #   new_name = "provide_rain_data"
  # )

  control <- provide_rain_data(
    user_id = user_id,
    spot_id = spot_id,
    sampling_time = sampling_time,
    date_range = date_range,
    info = FALSE
  )

  while (control$remaining > 0) {

    control <- provide_rain_data(control = control)
  }

  create_result(
    data = control$rain_ids,
    success = TRUE,
    message = sprintf(
      "%d rain data records have been inserted to the database.",
      length(control$rain_ids)
    )
  )
}

# provide_rain_data ------------------------------------------------------------

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
#' @param blocksize number of Radolan files to be downloaded and processed
#'   "at once", i.e. this number of files is downloaded, read, cropped, averaged
#'   and written to the database before loading new files. By doing so, at least
#'   some data will be available in the database in case that too many files
#'   cause a crash.
#' @param control object returned by a former call to this function that
#'   contains the current state of the import process and takes care that the
#'   next block of required data is downloaded. When omitted, the function
#'   returns an object that can be used as a control object for a next call of
#'   this function. See example.
#' @param info if \code{TRUE} (the default), a message is shown that describes
#'   how to use this function in a loop
#' @return vector of integer containing the IDs of the records inserted into the
#'   "rains" database table.
#' @export
#' @examples
#' \dontrun{
#' control <- provide_rain_data_for_bathing_spot(user_id = 5, spot_id = 41)
#' while (control$remaining > 0) {
#'   control <- provide_rain_data_for_bathing_spot(control = control)
#' }
#' }
provide_rain_data <- function(
  user_id, spot_id, sampling_time = "1050", date_range = NULL, blocksize = 10,
  control = NULL, info = TRUE
)
{
  #kwb.utils::assignPackageObjects("fhpredict")
  #user_id=5;spot_id=41;sampling_time="1050";date_range=NULL;blocksize=10;control=NULL

  if (is.null(control)) {

    # Determine the URLs to the Radolan files that are required to calibrate
    # a model. For each day of measurement six files (one for the day of
    # measurements and five for the five days before) are required.
    urls <- get_radolan_urls_for_measurements(
      user_id = user_id,
      spot_id = spot_id,
      sampling_time = sampling_time,
      date_range = date_range,
      all_in_range = FALSE,
      n_days_before = 5
    )

    # if (length(urls) == 0) {
    #   return(NULL)
    # }

    # Get metadata about the current bathing spot. Convert the area list structure
    # to a matrix with columns "lon" and "lat". Convert area structure given in
    # coordinate reference system "crs_from" to polygons given in coordinate
    # reference system "crs_to"
    polygon <- get_polygon_for_bathing_spot(user_id, spot_id)

    # Get rain data that already exists in the database
    rain_db <- api_get_rain(user_id, spot_id)

    # Group URLs into blocks to be downloaded and inserted "at once"
    blocks <- kwb.utils::splitIntoFixSizedBlocks(
      data = kwb.utils::noFactorDataFrame(url = urls),
      blocksize = blocksize
    )

    if (info) {
      message(
        "Please use the returned object in a loop to perform the actual data ",
        "import,\nas in the following code:\n\n",
        sprintf(
          "control <- provide_rain_data_for_bathing_spot(%d, %d)\n\n",
          user_id, spot_id
        ),
        "while (control$remaining > 0) {\n",
        "  control <- provide_rain_data_for_bathing_spot(control = control)\n",
        "}"
      )
    }

    # Return the control object
    return(list(
      user_id = user_id,
      spot_id = spot_id,
      sampling_time = sampling_time,
      date_range = date_range,
      blocksize = blocksize,
      polygon = polygon,
      rain_db = rain_db,
      blocks = blocks,
      remaining = length(blocks)
    ))
  }

  # If we arrive here, a control object was given to the function

  # if (length(urls) == 0) {
  #   return(NULL)
  # }

  get_object <- function(name) kwb.utils::selectElements(control, name)

  blocks <- get_object("blocks")
  n_blocks <- length(blocks)

  if (n_blocks == 0) {
    message("No URL blocks to process.")
    return(list(remaining = 0))
  }

  i <- n_blocks - get_object("remaining") + 1

  if (i < 1 || i > n_blocks) {
    message("Invalid block index.")
    return(list(remaining = 0))
  }

  user_id <- get_object("user_id")
  spot_id <- get_object("spot_id")

  kwb.utils::catAndRun(
    sprintf("Importing rain data block %d/%d", i, n_blocks),
    newLine = 3,
    expr = {

      # Get URLs from the current block
      urls <- stats::setNames(blocks[[i]]$url, rownames(blocks[[i]]))

      # Provide rain data in a data frame
      rain <- read_radolan_data_within_polygon(urls, get_object("polygon"))

      # For the days returned in the new rain data frame, replace the
      # corresponding records that exist in the database with the new records
      rain_ids <- api_replace_rain(
        user_id = user_id,
        spot_id = spot_id,
        rain = rain,
        rain_db = get_object("rain_db"),
        time_string = sampling_time_to_time_string(get_object("sampling_time")),
        comment = paste("imported:", Sys.time())
      )

      control$rain_ids <- c(control$rain_ids, rain_ids)
    } # end of expression to be evaluated by catAndRun()
  )

  control$remaining <- control$remaining - 1

  if (control$remaining == 0) {
    check_rain_data_consistency(user_id, spot_id)
  }

  control
}

# get_polygon_for_bathing_spot -------------------------------------------------
get_polygon_for_bathing_spot <- function(user_id, spot_id)
{
  coordinates_to_polygon(
    lonlat = get_area_coordinates(api_get_bathingspot(user_id, spot_id)),
    crs_from = sp::CRS('+proj=longlat +datum=WGS84'),
    crs_to = kwb.dwd:::get_radolan_projection_string()
  )
}

# read_radolan_data_within_polygon ---------------------------------------------
read_radolan_data_within_polygon <- function(urls, polygon)
{
  # For each URL, read the file and crop the polygon
  list_of_cropped <- lapply(seq_along(urls), function(i) {

    message(sprintf(
      "Reading and cropping from %s (%d/%d) ...",
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

  data.frame(
    datum = dates,
    rain = as.numeric(aggregated) / 10
  )
}

# api_replace_rain -------------------------------------------------------------
api_replace_rain <- function(
  user_id, spot_id, rain, rain_db = NULL, time_string, comment = ""
)
{
  # Read existing rain data from database if not given
  rain_db <- kwb.utils::defaultIfNULL(rain_db, api_get_rain(user_id, spot_id))

  # Find IDs that relate to days for which new data are available
  if (nrow(rain_db)) {

    # Get IDs of records that exist for the days to be inserted
    date_strings <- format(rain_db$date, "%Y-%m-%d")
    ids <- rain_db$id[date_strings %in% as.character(rain$datum)]

    # Clear existing rain from the database
    if (length(ids)) {

      api_delete_rain(user_id = user_id, spot_id = spot_id, ids = ids)
    }
  }

  # Add rain data frame to the database
  api_add_rain(
    user_id, spot_id, rain, time_string = time_string, comment = comment
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

# check_rain_data_consistency --------------------------------------------------
check_rain_data_consistency <- function(user_id, spot_id)
{
  rain <- fhpredict::api_get_rain(user_id, spot_id)

  times <- kwb.utils::selectColumns(rain, "dateTime")

  stopifnot(! is.unsorted(times))
  stopifnot(! any(duplicated(times)))
  #plot(rain$dateTime, rain$value, type = "l")
}
