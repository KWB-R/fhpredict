# api_get_irradiances ----------------------------------------------------------
api_get_irradiances <- function(user_id, spot_id)
{
  api_get_timeseries(
    path_irradiances(user_id, spot_id), "irradiances", type = "irradiances"
  )
}

# api_delete_irradiances -------------------------------------------------------
api_delete_irradiances <- function(user_id, spot_id, ids = NULL)
{
  api_delete_timeseries(user_id, spot_id, path_irradiances, ids)
}

# api_add_irradiances ----------------------------------------------------------
api_add_irradiances <- function(user_id, spot_id, data)
{
  path <- path_irradiances(user_id, spot_id)

  add_timeseries_to_database(path, data)
}
