# api_delete_timeseries --------------------------------------------------------
api_delete_timeseries <- function(
  user_id, spot_id, path_function, ids = NULL, subject = "timeseries",
  dbg = TRUE
)
{
  # Request a token to be reused
  token <- get_postgres_api_token()

  # Get all available ids if no ids are given
  if (is.null(ids)) {

    path <- path_function(user_id, spot_id)

    df <- api_get_timeseries(path, subject, token = token)

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

        path <- path_function(user_id, spot_id, id)

        safe_postgres_delete(path, token = token)
      }
    }
  )
}
