#
# Read rain data of a time period, crop polygons and average over each cell
#

user_id <- 3
spot_id <- 1441 # Kleine Badewiese
sampling_time <- "1050"

# Read rain data (averaged over cells within  bathing spot related polygon) for
# a certain time period
radolan_stack <- fhpredict:::read_radolan_raster_stack(
  start_year = 20170510,
  end_year = 20170515,
  bathing_season_only = TRUE,
  sampling_time = sampling_time
)

# Show the raster images
raster::plot(radolan_stack)

# Get metadata about the current bathing spot
spot <- fhpredict::api_get_bathingspot(spot_id = spot_id)

# Check the name of the bathing spot
spot$nameLong

# Let the user select a polygon around the bathing spot using an R Shiny app
#select_relevant_rain_area(lng = spot$longitude, lat = spot$latitude)
#area_from_app <- rain_area

# Provide the polygon in the same structure as returned by
# select_relevant_rain_area()
area_from_db <- fhpredict:::convert_area_structure(spot_area = spot$area)

#str(area_from_app)
str(area_from_db)

#area <- area_from_app
area <- area_from_db

# Crop the polygons from each raster layer
cropped <- fhpredict:::crop_area_from_radolan_stack(area, radolan_stack)

plot(cropped, add = TRUE)

# Get the mean over all layers for each point on the raster
aggregated <- raster::cellStats(cropped, stat = mean)

# The day information can be restored from the names of the layers
dates <- as.Date(substr(names(radolan_stack), 2, 9), format = "%Y%m%d")

rain_df <- data.frame(
  datum = dates,
  rain = as.numeric(aggregated) / 10
)

str(structure(rain_df, units = list(rain = "mm/h")))

# Add rain data frame to the database
rain_ids <- fhpredict::api_add_rain(user_id, spot_id, rain_df)

# Read rain from database
rain <- fhpredict::api_get_rain(user_id, spot_id)

# What should we do with duplicates?
sum(duplicated(kwb.utils::pasteColumns(rain, c("date", "dateTime"))))

# Delete rain data with given rain record ids
fhpredict::api_delete_rain(user_id, spot_id, rain_ids)

# For simplicity reasons: delete all rain data before inserting new data
fhpredict::api_delete_rain(user_id, spot_id)

# TODO: Remove only the duplicates, keeping the most current values
