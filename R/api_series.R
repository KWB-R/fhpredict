# api_add_series ---------------------------------------------------------------
api_add_series <- function(path_function, user_id, spot_id, name)
{
  result <- safe_postgres_post(
    path = path_function(user_id, spot_id),
    body = list(name = name)
  )

  result$data[[1]]$id
}

# api_delete_series ------------------------------------------------------------
api_delete_series <- function(
  get_function, path_function, user_id, spot_id, series_id = NULL
)
{
  if (is.null(series_id)) {
    series_id <- get_function(user_id, spot_id)$id
  }

  for (id in series_id) {
    kwb.utils::catAndRun(
      paste("Deleting series", id),
      safe_postgres_delete(path_function(user_id, spot_id, id))
    )
  }
}

# api_get_series ---------------------------------------------------------------
api_get_series <- function(path_function, user_id, spot_id, series_id = -1L)
{
  path <- path_function(user_id, spot_id, series_id)

  result <- safe_postgres_get(path)

  if (series_id == -1L) {
    flatten_recursive_list(result$data)
  } else {
    result$data
  }
}

# api_add_series_measurements -------------------------------------------------
api_add_series_measurements <- function(
  path_function, user_id, spot_id, series_id, data
)
{
  add_timeseries_to_database(path_function(user_id, spot_id, series_id), data)
}

# api_get_series_measurements --------------------------------------------------
api_get_series_measurements <- function(
  path_function, user_id, spot_id, series_id, subject
)
{
  path <- path_function(user_id, spot_id, series_id)

  data <- api_get_timeseries(path, subject = paste(subject, "measurements"))

  remove_and_reorder_columns(data, paste0(subject, "_measurements"))
}

# api_delete_series_measurements -----------------------------------------------
api_delete_series_measurements <- function(
  path_function, user_id, spot_id, series_id, ids = NULL, subject = "series",
  ...
)
{
  api_delete_timeseries(
    user_id,
    spot_id,
    series_id = series_id,
    ids = ids,
    path_function = path_function,
    subject = paste(subject, "measurements"),
    ...
  )
}
