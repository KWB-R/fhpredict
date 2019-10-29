# get_area_coordinates ---------------------------------------------------------

#' Return the coordinates of area assigned to bathing spot
#'
#' @param bathing_spot list as returned by the API for one bathing spot
#' @return matrix with two columns and as many rows as there are points
#'   describing the area
get_area_coordinates <- function(bathing_spot)
{
  #fhpredict:::check_bathingspot(bathing_spot)

  area <- kwb.utils::selectElements(bathing_spot, "area")

  coordinates_list <- kwb.utils::selectElements(area, "coordinates")
  type <- kwb.utils::selectElements(area, "type")

  if (length(coordinates_list) == 0) {
    return(structure(matrix(numeric(0), nrow = 0, ncol = 2), type = type))
  }

  points <- coordinates_list[[1]]

  coordinates <- do.call(rbind, lapply(points, function(point) {
    stopifnot(length(point) == 2)
    c(point[[1]], point[[2]])
  }))

  structure(coordinates, type = type)
}
