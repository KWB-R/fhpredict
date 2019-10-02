# Set a global user
user_id <- 5

# Check rain data availability -------------------------------------------------
if (FALSE)
{
  files_per_day <- fhpredict:::get_radolan_url_frequency()
  n_files <- kwb.utils::removeColumns(files_per_day, c("2013", "2014"))[, -1]
  kwb.utils::resetRowNames(files_per_day[rowSums(n_files != 24) > 0, ])
}

# Test read_radolan_raster_stack() ---------------------------------------------
if (FALSE)
{
  # Get URLs to Radolan files
  urls <- fhpredict::get_radolan_urls_bucket(
    from = "20110101",
    to = "20120101",
    time = "1050",
    bathing_season_only = TRUE
  )

  system.time(stack <- fhpredict:::read_radolan_raster_stack(urls))
  # 1 year (bathing season) -> ca. 3 min
}

#date_from = "20190701";date_to = "20190706";sampling_time = "1050";bathing_season_only = TRUE

# Where do I find measurements? ------------------------------------------------
if (FALSE)
{
  spots <- fhpredict::api_get_bathingspot(user_id)

  measurements <- lapply(stats::setNames(nm = spots$id), function(spot_id) {
    fhpredict::api_get_bathingspot(user_id, spot_id)$measurements
  })

  (spot_ids <- spots$id[lengths(measurements) > 0])

  #lapply(spot_ids, fhpredict:::get_unique_measurement_dates, user_id = user_id)
}

# Test the performance of getting Radolan URLs in different range "slices" -----
if (FALSE)
{
  spot_id <- 41

  # Get the dates for which E. coli measurements are available
  dates_all <- fhpredict:::get_unique_measurement_dates(user_id, spot_id)

  # Reduce to dates within the bathing season
  dates <- dates_all[fhpredict:::is_in_bathing_season(dates_all)]

  # Add up to five days before each date
  dates_5d_before <- fhpredict:::add_days_before(dates, 5)

  # Test the performance of calling fhpredict::get_radolan_urls_bucket()
  fhpredict:::test_performance_get_radolan_urls(dates = dates_5d_before)

  # Get URLs to related Radolan files
  urls <- fhpredict:::get_radolan_urls_for_days(dates_5d_before)

  remove_dash <- function(x) gsub("-", "", x)

  # URLs determined with the original approach
  urls_old <- fhpredict:::get_radolan_urls_bucket(
    from = remove_dash(min(dates_5d_before)),
    to = remove_dash(max(dates_5d_before)),
    time = "1050",
    bathing_season_only = TRUE
  )

  # Show number of dates in each vector
  numbers <- t(data.frame(
    n_days_all = length(dates_all),
    n_days_bathing_season = length(dates),
    n_days_in_5day_ranges = length(dates_5d_before),
    # Number of Radolan files that will be downloaded with the new approach
    n_radolan_files_new = length(urls),
    # Number of Radolan files that were downloaded with the original approach
    n_radolan_files_old = length(urls_old)
  ))

  # Print the numbers
  numbers

  # Expected gain in performance
  unname(numbers["n_radolan_files_new", ] / numbers["n_radolan_files_old", ])
}

# Test provide_rain_data_for_bathing_spot() ------------------------------------
if (FALSE)
{
  user_id <- 4
  spot_id <- 15

  #spot <- fhpredict::api_get_bathingspot(user_id, spot_id)
  #fhpredict:::api_get_rain(user_id, spot_id)

  result <- fhpredict::provide_rain_data_for_bathing_spot(
    user_id, spot_id
    #, date_range = c("2008-07-01", "2013-01-01")
  )
}
