# api_add_discharge ------------------------------------------------------------
api_add_discharge <- function(user_id, spot_id, discharge, comment = NULL)
{
  stopifnot(is.data.frame(discharge))

  # Provide vector of timestamps
  timestamps <- as.character(kwb.utils::selectColumns(discharge, "dateTime"))

  data <- kwb.utils::noFactorDataFrame(
    date = substr(timestamps, 1, 10),
    dateTime = substr(timestamps, 12, 19),
    value = kwb.utils::selectColumns(discharge, "discharge")
  )

  data$comment <- comment

  # Return the ids of the discharge data records
  add_timeseries_to_database(
    path = path_discharges(user_id, spot_id),
    data = data
  )
}

# api_get_discharge ------------------------------------------------------------

#' Get Discharge Data from the Postgres Database Via API
#'
#' @param user_id user id
#' @param spot_id bathing spot id
#' @export
#'
api_get_discharge <- function(user_id, spot_id)
{
  api_get_timeseries(
    path = path_discharges(user_id, spot_id),
    subject = "discharge"
  )
}

# api_delete_discharge ---------------------------------------------------------

#' Delete Discharge Data from Postgres Database Via API
#'
#' @param user_id user id
#' @param spot_id bathing spot id
#' @param ids optional. Vector of discharge ids. If not given or \code{NULL}
#'   (the default) all discharge data for the bathing spot are deleted!
#' @param dbg if \code{TRUE} debug messages are shown
#' @export
#'
api_delete_discharge <- function(user_id, spot_id, ids = NULL, dbg = TRUE)
{
  api_delete_timeseries(
    user_id = user_id,
    spot_id = spot_id,
    ids = ids,
    path_function = path_discharges,
    subject = "discharge"
  )
}
