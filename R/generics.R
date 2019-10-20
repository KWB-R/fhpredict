# api_add_generic --------------------------------------------------------------
api_add_generic <- function(user_id, spot_id, name)
{
  result <- safe_postgres_post(
    path = path_generics(user_id, spot_id),
    body = list(name = name)
  )

  result$data[[1]]$id
}

# api_delete_generic -----------------------------------------------------------
api_delete_generic <- function(user_id, spot_id, generic_id = NULL)
{
  if (is.null(generic_id)) {
    generic_id <- api_get_generic(user_id, spot_id)$id
  }

  for (id in generic_id) {
    kwb.utils::catAndRun(
      paste("Deleting generic", id),
      safe_postgres_delete(path_generics(user_id, spot_id, id))
    )
  }
}

# api_get_generic --------------------------------------------------------------
api_get_generic <- function(user_id, spot_id, generic_id = -1L)
{
  path <- path_generics(user_id, spot_id, generic_id)

  result <- safe_postgres_get(path)

  if (generic_id == -1L) {
    flatten_recursive_list(result$data)
  } else {
    result$data
  }
}

# api_add_generic_measurements -------------------------------------------------
api_add_generic_measurements <- function(user_id, spot_id, generic_id, data)
{
  path <- path_generic_measurements(user_id, spot_id, generic_id)
  add_timeseries_to_database(path, data)
}
