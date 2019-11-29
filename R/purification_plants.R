# api_add_plant ----------------------------------------------------------------
api_add_plant <- function(user_id, spot_id, name)
{
  api_add_series(path_plants, user_id, spot_id, name)
}

# api_delete_plant -------------------------------------------------------------
api_delete_plant <- function(user_id, spot_id, plant_id = NULL)
{
  api_delete_series(
    api_get_plant, path_plants, user_id, spot_id, plant_id
  )
}

# api_get_plant ----------------------------------------------------------------
api_get_plant <- function(user_id, spot_id, plant_id = -1L)
{
  api_get_series(path_plants, user_id, spot_id, plant_id)
}

# api_add_plant_measurements ---------------------------------------------------
api_add_plant_measurements <- function(user_id, spot_id, plant_id, data)
{
  api_add_series_measurements(
    path_plant_measurements, user_id, spot_id, plant_id, data
  )
}

# api_get_plant_measurements ---------------------------------------------------
api_get_plant_measurements <- function(user_id, spot_id, plant_id)
{
  api_get_series_measurements(
    path_plant_measurements, user_id, spot_id, plant_id, subject = "plant"
  )
}

# api_delete_plant_measurements ------------------------------------------------
api_delete_plant_measurements <- function(
  user_id, spot_id, plant_id, ids = NULL, ...
)
{
  api_delete_series_measurements(
    path_plant_measurements, user_id, spot_id, plant_id, ids, subject = "plant",
    ...
  )
}
