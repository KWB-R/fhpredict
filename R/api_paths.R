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
  path <- paste0(path_bathingspot(user_id, spot_id), "/discharges")

  if (discharge_id == -1L) {
    return(path)
  }

  sprintf("%s/%d", path, discharge_id)
}

# path_measurements ------------------------------------------------------------
path_measurements <- function(user_id, spot_id)
{
  paste0(path_bathingspot(user_id, spot_id), "/measurements")
}

# path_models ------------------------------------------------------------------
path_models <- function(user_id, spot_id, model_id = -1L)
{
  path <- paste0(path_bathingspot(user_id, spot_id), "/models")

  if (model_id == -1L) {
    return(path)
  }

  sprintf("%s/%d", path, model_id)
}

# path_predictions -------------------------------------------------------------
path_predictions <- function(user_id, spot_id, prediction_id = -1L)
{
  path <- paste0(path_bathingspot(user_id, spot_id), "/predictions")

  if (prediction_id == -1L) {
    return(path)
  }

  sprintf("%s/%d", path, prediction_id)
}

# path_rains -------------------------------------------------------------------
path_rains <- function(user_id, spot_id, rain_id = -1L)
{
  path <- paste0(path_bathingspot(user_id, spot_id), "/rains")

  if (rain_id == -1L) {
    return(path)
  }

  sprintf("%s/%d", path, rain_id)
}
