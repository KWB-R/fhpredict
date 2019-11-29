# append_id_if_given -----------------------------------------------------------
append_id_if_given <- function(path, id = -1L)
{
  if (id == -1L) {
    return(path)
  }

  paste0(path, "/", id)
}

# path_bathingspot -------------------------------------------------------------

#' Path to Bathing Spot API endpoint
#'
#' @keywords internal
#' @examples
#' fhpredict:::path_bathingspot()
#' fhpredict:::path_bathingspot(spot_id = 3)
#' fhpredict:::path_bathingspot(user_id = 7)
#' fhpredict:::path_bathingspot(user_id = 22, spot_id = 33)
path_bathingspot <- function(
  user_id = -1L, spot_id = -1L, limit = NULL, skip = NULL, sep = "/"
)
{
  # Helper function to provide "key=value" string or NULL
  to_param <- function(x) if (! is.null(x)) {
    paste0(deparse(substitute(x)), "=", x)
  }

  # Create parameter string
  params <- paste(c(to_param(skip), to_param(limit)), collapse = "&")

  paste0(
    ifelse(user_id == -1L, "", paste0("users", sep, user_id, sep)),
    "bathingspots",
    ifelse(spot_id == -1L, "", paste0(sep, spot_id)),
    if (nzchar(params)) paste0("?", params)
  )
}

# path_discharges --------------------------------------------------------------
path_discharges <- function(user_id, spot_id, discharge_id = -1L)
{
  path_general("discharges", user_id, spot_id, discharge_id)
}

# path_general -----------------------------------------------------------------
path_general <- function(type, user_id, spot_id, id)
{
  path <- paste0(path_bathingspot(user_id, spot_id), "/", type)

  append_id_if_given(path, id)
}

# path_generics ----------------------------------------------------------------
path_generics <- function(user_id, spot_id, generic_id = -1L)
{
  path_general("genericInputs", user_id, spot_id, generic_id)
}

# path_generic_measurements ----------------------------------------------------
path_generic_measurements <- function(user_id, spot_id, generic_id, id = -1L)
{
  path_sub_measurements(path_generics, user_id, spot_id, generic_id, id)
}

# path_irradiances -------------------------------------------------------------
path_irradiances <- function(user_id, spot_id, irradiance_id = -1L)
{
  path_general("globalIrradiances", user_id, spot_id, irradiance_id)
}

# path_measurements ------------------------------------------------------------
path_measurements <- function(user_id, spot_id, measurement_id = -1L)
{
  path_general("measurements", user_id, spot_id, measurement_id)
}

# path_models ------------------------------------------------------------------
path_models <- function(user_id, spot_id, model_id = -1L)
{
  path_general("models", user_id, spot_id, model_id)
}

# path_plants ------------------------------------------------------------------
path_plants <- function(user_id, spot_id, plant_id = -1L)
{
  path_general("purificationPlants", user_id, spot_id, plant_id)
}

# path_plant_measurements ------------------------------------------------------
path_plant_measurements <- function(user_id, spot_id, plant_id, id = -1L)
{
  path_sub_measurements(path_plants, user_id, spot_id, plant_id, id)
}

# path_predictions -------------------------------------------------------------
path_predictions <- function(user_id, spot_id, prediction_id = -1L)
{
  path_general("predictions", user_id, spot_id, prediction_id)
}

# path_rains -------------------------------------------------------------------
path_rains <- function(user_id, spot_id, rain_id = -1L)
{
  path_general("rains", user_id, spot_id, rain_id)
}

# path_sub_measurements --------------------------------------------------------
path_sub_measurements <- function(
  path_function, user_id, spot_id, series_id, id = -1L
)
{
  path <- paste0(path_function(user_id, spot_id, series_id), "/measurements")

  append_id_if_given(path, id)
}
