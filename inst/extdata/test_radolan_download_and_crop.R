if (FALSE)
{
  # Function to be triggered by the user: Provide rain data for the bathing spot
  system.time(rain_ids <- fhpredict::provide_rain_data_for_bathing_spot(
    user_id = 5, spot_id = 41, sampling_time = "1050"
    #, date_range = as.Date(c("2008-07-01", "2009-07-01"))
  ))

  # Read rain from database
  rain <- fhpredict::api_get_rain(user_id, spot_id)

  View(rain)

  # What should we do with duplicates?
  sum(duplicated(kwb.utils::pasteColumns(rain, c("date", "dateTime"))))

  # Delete rain data with given rain record ids
  fhpredict::api_delete_rain(user_id, spot_id, rain_ids)

  # For simplicity reasons: delete all rain data before inserting new data
  fhpredict::api_delete_rain(user_id, spot_id)

  # TODO: Remove only the duplicates, keeping the most current values
}

# Stack cropped instead of cropping stacked ------------------------------------
if (FALSE)
{
  # Define URLs to two Radolan files
  urls <- paste0(
    "https://flusshygiene-radolan-sf-data.s3.eu-central-1.amazonaws.com/",
    c("18/09/13/raa01-sf_10000-1809131050-dwd---bin",
      "18/09/14/raa01-sf_10000-1809141050-dwd---bin")
  )

  # Get bathing spot object
  spot <- fhpredict::api_get_bathingspot(3, 43)

  # Create lonlat table, indirectly
  area <- fhpredict:::convert_area_structure(spot_area = spot$area)
  lonlat1 <- fhpredict:::area_to_longitude_latitude_matrix(area)

  # Create lonlat table, direct
  lonlat2 <- fhpredict:::get_area_coordinates(spot)

  # Check if the coordinates are the same in both tables
  stopifnot(all(lonlat1 == lonlat2))

  # Read both files individually
  radolan1a <- kwb.dwd::read_binary_radolan_file(urls[1])
  radolan1b <- kwb.dwd::read_binary_radolan_file(urls[2])

  # Read both files and stack them with a function from fhpredict
  radolan2 <- fhpredict:::read_radolan_raster_stack(urls)

  # Crop the areas from the stack (problematic if there are many layers!)
  c2 <- fhpredict:::crop_area_from_radolan_stack(area, radolan2)

  radolan_proj <- kwb.dwd:::get_radolan_projection_string()

  identical(radolan_proj, as.character(raster::crs(radolan1a)))

  # Convert the area list structure to a matrix with columns "lon" and "lat"
  # Convert area structure given in coordinate reference system "crs_from"
  # to polygons given in coordinate reference system "crs_to"
  polygon <- fhpredict:::coordinates_to_polygon(
    lonlat = lonlat2,
    crs_from = sp::CRS('+proj=longlat +datum=WGS84'),
    crs_to = radolan_proj
  )

  # Do the steps of crop_area_from_radolan_stack() separately
  c1a <- raster::crop(radolan1a, polygon)
  c1b <- raster::crop(radolan1b, polygon)

  c1 <- raster::stack(list(c1a, c1b))

  identical(raster::cellStats(c1, "mean"), raster::cellStats(c2, "mean"))

  fhpredict:::provide_rain_data_for_bathing_spot
}
