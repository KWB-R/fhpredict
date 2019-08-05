# path_bathingspot -------------------------------------------------------------
#' @examples
#' path_bathingspot()
#' path_bathingspot(spot_id = 3)
#' path_bathingspot(user_id = 7)
#' path_bathingspot(user_id = 22, spot_id = 33)
path_bathingspot <- function(user_id = -1L, spot_id = -1L, sep = "/")
{
  path_id_or_not <- function(part, id) {
    ifelse(id == -1L, "", sprintf("%s%s%d%s", part, sep, id, sep))
  }

  id_or_not <- function(id) {
    ifelse(id == -1L, "", paste0(sep, id))
  }

  paste0(path_id_or_not("users", user_id), "bathingspots", id_or_not(spot_id))
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
