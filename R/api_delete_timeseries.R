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

      message(get_text(
        "no_data", subject = subject, user_id = user_id, spot_id = spot_id
      ))

      return()
    }

    ids <- kwb.utils::selectColumns(df, "id")
  }

  # Delete all records given their ids
  kwb.utils::catAndRun(
    get_text("deleting_data_points", n = length(ids), subject = subject),
    dbg = dbg, {
      for (id in ids) {
        safe_postgres_delete(path = path_function(user_id, spot_id, id))
      }
    }
  )
}
