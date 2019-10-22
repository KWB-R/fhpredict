# crop_area_from_radolan_stack -------------------------------------------------
crop_area_from_radolan_stack <- function(area, radolan_stack, use_mask = TRUE)
{
  #kwb.utils::assignPackageObjects("fhpredict")

  # Convert the area list structure to a matrix with columns "lon" and "lat"
  lonlat <- area_to_longitude_latitude_matrix(area)

  # Convert area structure given in coordinate reference system "crs_from"
  # to polygons given in coordinate reference system "crs_to"
  polygon <- coordinates_to_polygon(
    lonlat = lonlat,
    crs_from = sp::CRS('+proj=longlat +datum=WGS84'),
    crs_to = as.character(raster::crs(radolan_stack))
  )

  # Crop or mask the polygon areas from the raster stack
  crop_or_mask(radolan_stack, polygon, use_mask)
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

# coordinates_to_polygon -------------------------------------------------------
coordinates_to_polygon <- function(lonlat, crs_from, crs_to)
{
  # Create spatial polygon object from coordinates
  polygons_spatial <- raster::spPolygons(lonlat, crs = crs_from)

  # Convert spatial polygon object to simple feature object
  polygons_sf <- sf::st_as_sf(polygons_spatial)

  # Transform the coordinates of the simple feature object
  sf::st_transform(x = polygons_sf, crs = crs_to)
}
