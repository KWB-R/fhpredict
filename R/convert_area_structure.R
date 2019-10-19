# convert_area_structure -------------------------------------------------------
convert_area_structure <- function(spot_area, swap_lat_long = FALSE)
{
  # Extract the element "type"
  area_type <- kwb.utils::selectElements(spot_area, "type")

  # Extract the element "coordinates"
  coordinates <- kwb.utils::selectElements(spot_area, "coordinates")

  # We expect a polygon
  stopifnot(identical(area_type, "Polygon"))

  # We expect the coordinates to be a list of length one
  stopifnot(length(coordinates) == 1)

  # Select the first and only element
  coords <- coordinates[[1]]

  # Interchange latitude and longitude in the coordinates if requested
  if (swap_lat_long) {
    coords <- lapply(coords, function(x) x[c(2, 1)])
  }

  # Return a list similar to what select_relevant_rain_area() returns
  list(geometry = list(type = "Polygon", coordinates = list(coords)))
}
