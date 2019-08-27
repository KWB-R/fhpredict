# MAIN -------------------------------------------------------------------------
radolan_stack <- fhpredict:::read_radolan_raster_stack(
  start_year = 20170510,
  end_year = 20170515,
  bathing_season_only = TRUE,
  sampling_time = "1050"
)

# The day information can be restored from the names of the layers
as.Date(substr(names(radolan_stack), 2, 9), format = "%Y%m%d")

# Select Kleine Badewiese
spot_id <- 1441
spot <- fhpredict::api_get_bathingspot(spot_id = spot_id)
spot$nameLong

# Latitude and longitude are interchanged in the database!!!
#select_relevant_rain_area(lng = spot$latitude, lat = spot$longitude)

area_from_app <- rain_area
area_from_db <- fhpredict:::convert_area_structure(spot_area = spot$area)

area <- area_from_db

str(area_from_app)
str(area_from_db)

aggregated <- crop_area_from_radolan_stack(
  area = area,
  radolan_stack = radolan_stack,
  stat_fun = mean
)

rain_df <- data.frame(
  datum = lubridate::ymd(substr(names(aggregated), 2, 7)),
  rain = as.numeric(aggregated) / 10
)
