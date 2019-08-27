crop_rain_area_from_radolan <- function(area,
                                        radolan_stack,
                                        stat_fun
)
{
  # extracting and clean coords from area object
  coord <- area$geometry$coordinates
  names(coord[[1]]) <- paste0("point_", as.character(1:length(coord[[1]])))
  coord <- bind_rows(coord)

  for(i in names(coord))
    names(coord[[i]]) <- c("lng", "lat")

  # defining coord vectors for polygon
  lng <- c()
  lat <- c()

  for(i in 1:length(names(coord)))
  {
    lng[i] <- coord[[i]][[1]]
    lat[i] <- coord[[i]][[2]]
  }

  lonlat <-cbind(lng, lat)
  crdref <- CRS('+proj=longlat +datum=WGS84')

  pols <- spPolygons(lonlat, crs = crdref)
  pols <- sf::st_as_sf(pols)

  pols <- sf::st_transform(x = pols, crs = as.character(crs(radolan_stack)))
  y <<- raster::crop(x = radolan_stack, y = pols)

  x <- raster::cellStats(y, stat_fun)
  data.frame(SAMPLE_DATE = lubridate::ymd(substr(names(x),
                                                 2, 7)), rain_mean = as.numeric(x))



}
