# api_add_rain -----------------------------------------------------------------

#' Add Rain Data Frame to Postgres Database Via API
#'
#' @param user_id user id
#' @param spot_id bathing spot id
#' @param rain data frame containing rain data (columns \code{datum},
#'   \code{rain})
#' @param time_string time to which rain data values relate, default:
#'   \code{"12:00:00"}
#' @param comment character string to be stored in database field "comment"
#' @export
#' @examples
#' \dontrun{
#' # Define some fake rain data
#' rain <- data.frame(
#'   datum = as.Date(c("2019-08-29", "2019-08-30")),
#'   rain = c(1.23, 2.34)
#' )
#'
#' # Add the rain data to the database
#' rain_ids <- api_add_rain(user_id = 3, spot_id = 1441, rain)
#'
#' Show the IDs of the created database records
#' rain_ids
#'
#' # Delete the fake rain data
#' api_delete_rain(user_id = 3, spot_id = 1441, rain_ids = rain_ids)
#' }
#'
api_add_rain <- function(
  user_id, spot_id, rain, time_string = "12:00:00", comment = NULL
)
{
  stopifnot(is.data.frame(rain))

  # Provide vectors of dates and values
  date_strings <- as.character(kwb.utils::selectColumns(rain, "datum"))
  values <- kwb.utils::selectColumns(rain, "rain")

  result <- lapply(seq_along(values), function(i) {
    add_rain_datapoint_to_database(
      user_id,
      spot_id,
      date_string = date_strings[i],
      time_string = time_string,
      value = values[i],
      comment = comment
    )
  })

  # Return the ids of the rain data records
  unlist(result)
}

# add_rain_datapoint_to_database -----------------------------------------------
add_rain_datapoint_to_database <- function(
  user_id, spot_id, date_string, time_string, value, comment = NULL
)
{
  result <- postgres_post(
    path = path_rains(user_id, spot_id),
    body = list(
      value = value,
      dateTime = time_string,
      date = date_string,
      comment = comment
    )
  )

  stop_on_request_failure(result)

  rain_id <- result$data[[1]]$id

  message("A rain data record with id = ", rain_id, " has been inserted.")

  # Return the id of the added record
  rain_id
}

# api_get_rain -----------------------------------------------------------------

#' Get Rain Data from the Postgres Database Via API
#'
#' @param user_id user id
#' @param spot_id bathing spot id
#' @export
#'
api_get_rain <- function(user_id, spot_id)
{
  rain <- kwb.utils::catAndRun("Reading rain data from database", {

    result <- postgres_get(path = path_rains(user_id, spot_id))

    stop_on_request_failure(result)

    flatten_recursive_list(result$data)
  })

  if (is.null(rain)) {

    return(data.frame())
  }

  rain <- kwb.utils::catAndRun("Converting time columns from text to POSIXct", {
    rain <- convert_time_columns(rain)
    rain$dateTime = as.POSIXct(
      x = kwb.utils::pasteColumns(rain, c("date", "dateTime")),
      tz = "Europe/Berlin"
    )
    rain
  })

  kwb.utils::removeColumns(rain, c("date", "comment"))
}

# api_delete_rain --------------------------------------------------------------

#' Delete Rain Data from Postgres Database Via API
#'
#' @param user_id user id
#' @param spot_id bathing spot id
#' @param rain_ids optional. Vector of rain ids. If not given or \code{NULL}
#'   (the default) all rain data for the bathing spot are deleted!
#' @param dbg if \code{TRUE} debug messages are shown
#' @export
#'
api_delete_rain <- function(user_id, spot_id, rain_ids = NULL, dbg = TRUE)
{
  # Get all available rain ids if no ids are given
  if (is.null(rain_ids)) {

    rain <- api_get_rain(user_id, spot_id)

    if (nrow(rain) == 0) {

      message(
        sprintf(
          "No rain data available for user_id = %d, spot_id = %d. ",
          user_id, spot_id
        ),
        "Nothing to delete."
      )

      return()
    }

    rain_ids <- kwb.utils::selectColumns(rain, "id")
  }

  # Delete all records given their rain ids
  for (rain_id in rain_ids) {

    kwb.utils::catAndRun(
      paste("Deleting rain data point with id", rain_id),
      dbg = dbg, {
        result <- postgres_delete(path = path_rains(user_id, spot_id, rain_id))
        stop_on_request_failure(result)
      }
    )
  }
}
