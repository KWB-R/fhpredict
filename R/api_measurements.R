# api_get_measurements --------------------------------------------------------
api_get_measurements <- function(user_id = -1, spot_id = -1)
{
  path <- path_measurements(user_id, spot_id)

  result <- safe_postgres_get(path)

  measurements <- kwb.utils::safeRowBindAll(lapply(result$data, function(x) {
    kwb.utils::asNoFactorDataFrame(kwb.utils::excludeNULL(x, dbg = FALSE))
  }))

  measurements <- kwb.utils::removeColumns(measurements, pattern = "At$")

  measurements[order(kwb.utils::selectColumns(measurements, "date")), ]
}

# api_delete_measurements ------------------------------------------------------

#' Delete Discharge Data from Postgres Database Via API
#'
#' @param user_id user id
#' @param spot_id bathing spot id
#' @param ids optional. Vector of measurement ids. If not given or \code{NULL}
#'   (the default) all measurements for the bathing spot are deleted!
#' @param dbg if \code{TRUE} debug messages are shown
#' @export
#'
api_delete_measurements <- function(user_id, spot_id, ids = NULL, dbg = TRUE)
{
  api_delete_timeseries(
    user_id = user_id,
    spot_id = spot_id,
    ids = ids,
    path_function = path_measurements,
    subject = "measurement"
  )
}
