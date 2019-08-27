# crop_area_from_radolan_stack -------------------------------------------------
crop_area_from_radolan_stack <- function(area, radolan_stack, stat_fun)
{
  # Convert the area list structure to a matrix with columns "lon" and "lat"
  lonlat <- area_to_longitude_latitude_matrix(area)

  # Define the coordinate reference system of the polygon coordinates
  crs_polygon <- sp::CRS('+proj=longlat +datum=WGS84')

  # Create spatial polygon objects from coordinates
  polygons_spatial <- raster::spPolygons(lonlat, crs = crs_polygon)

  # Convert spatial polygon objects to simple feature objects
  polygons_sf <- sf::st_as_sf(polygons_spatial)

  # Get the coordinate reference system of the radolan raster stack
  crs_radolan <- as.character(raster::crs(radolan_stack))

  # Transform the coordinates of the simple feature object
  polygons_transformed <- sf::st_transform(x = polygons_sf, crs = crs_radolan)

  # Crop the polygon areas from the raster stack
  cropped <- raster::crop(x = radolan_stack, y = polygons_transformed)

  # Aggregate the cells of each raster point using the statistics function
  raster::cellStats(cropped, stat_fun)
}

# area_to_longitude_latitude_matrix --------------------------------------------
area_to_longitude_latitude_matrix <- function(area)
{
  # Select the "geometry" element or stop
  geometry <- kwb.utils::selectElements(area, "geometry")

  # Select the "coordinates" element or stop
  coordinates <- kwb.utils::selectElements(geometry, "coordinates")

  # We expect the list of coordinates to have a length of one
  stopifnot(length(coordinates) == 1)

  # Get the first and only element of the list
  coord <- coordinates[[1]]

  # Convert the sublists into two-element vectors and rbind the vectors to a
  # matrix
  lonlat <- do.call(rbind, lapply(coord, function(x) c(x[[1]], x[[2]])))

  # Set column names
  colnames(lonlat) <- c("lon", "lat")

  # Return the matrix
  lonlat
}
