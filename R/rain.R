# api_add_rain -----------------------------------------------------------------

#' Add Rain Data Frame to Postgres Database Via API
#'
#' @param user_id user id
#' @param spot_id bathing spot id
#' @param rain data frame containing rain data (columns \code{datum},
#'   \code{rain})
#' @param time_string time to which rain data values relate, default:
#'   \code{"12:00:00"}
#' @param comment character string to be written to the field "comment" of the
#'   rain database table.
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
#' api_delete_rain(user_id = 3, spot_id = 1441, ids = rain_ids)
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

  kwb.utils::catAndRun(
    get_text("inserting_rain", n = length(date_strings)),
    expr = {

      # Prepare data frame to be passed to add_timeseries_to_database()
      data <- kwb.utils::noFactorDataFrame(
        date = date_strings,
        dateTime = time_string,
        value = values
      )

      data$comment <- comment

      add_timeseries_to_database(path_rains(user_id, spot_id), data)
    }
  )
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
  rain <- api_get_timeseries(path_rains(user_id, spot_id), subject = "rain")

  remove_and_reorder_columns(rain, "rain")
}

# api_delete_rain --------------------------------------------------------------

#' Delete Rain Data from Postgres Database Via API
#'
#' @param user_id user id
#' @param spot_id bathing spot id
#' @param ids optional. Vector of rain ids. If not given or \code{NULL} (the
#'   default) all rain data for the bathing spot are deleted!
#' @param dbg if \code{TRUE} debug messages are shown
#' @param \dots further arguments passed to
#'   \code{fhpredict:::api_delete_timeseries}
#' @export
#'
api_delete_rain <- function(user_id, spot_id, ids = NULL, dbg = TRUE, ...)
{
  api_delete_timeseries(
    user_id,
    spot_id,
    ids = ids,
    path_function = path_rains,
    subject = "rain",
    ...
  )
}
