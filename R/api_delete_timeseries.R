# api_delete_timeseries --------------------------------------------------------
api_delete_timeseries <- function(
  user_id, spot_id, path_function, ids = NULL, subject = "timeseries",
  dbg = TRUE
)
{
  # Get all available ids if no ids are given
  if (is.null(ids)) {

    df <- api_get_timeseries(path = path_function(user_id, spot_id), subject)

    if (nrow(df) == 0) {

      message(
        sprintf(
          "No %s data available for user_id = %d, spot_id = %d. ",
          subject, user_id, spot_id
        ),
        "Nothing to delete."
      )

      return()
    }

    ids <- kwb.utils::selectColumns(df, "id")
  }

  # Delete all records given their ids
  for (id in ids) {

    kwb.utils::catAndRun(
      paste("Deleting", subject, "data point with id", id),
      dbg = dbg, {
        result <- postgres_delete(path = path_function(user_id, spot_id, id))
        stop_on_request_failure(result)
      }
    )
  }
}
