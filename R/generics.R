# api_add_generic --------------------------------------------------------------
api_add_generic <- function(user_id, spot_id, name)
{
  api_add_series(path_generics, user_id, spot_id, name)
}

# api_delete_generic -----------------------------------------------------------
api_delete_generic <- function(user_id, spot_id, generic_id = NULL)
{
  api_delete_series(
    api_get_generic, path_generics, user_id, spot_id, generic_id
  )
}

# api_get_generic --------------------------------------------------------------
api_get_generic <- function(user_id, spot_id, generic_id = -1L)
{
  api_get_series(path_generics, user_id, spot_id, generic_id)
}

# api_add_generic_measurements -------------------------------------------------
api_add_generic_measurements <- function(user_id, spot_id, generic_id, data)
{
  api_add_series_measurements(
    path_generic_measurements, user_id, spot_id, generic_id, data
  )
}

# api_get_generic_measurements -------------------------------------------------
api_get_generic_measurements <- function(user_id, spot_id, generic_id)
{
  api_get_series_measurements(
    path_generic_measurements, user_id, spot_id, generic_id, subject = "generic"
  )
}

# api_delete_generic_measurements ----------------------------------------------
api_delete_generic_measurements <- function(
  user_id, spot_id, generic_id, ids = NULL, ...
)
{
  api_delete_series_measurements(
    path_generic_measurements, user_id, spot_id, generic_id, ids,
    subject = "generic", ...
  )
}
