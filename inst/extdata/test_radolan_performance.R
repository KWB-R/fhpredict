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
  system.time(stack <- fhpredict:::read_radolan_raster_stack(
    date_from = "20110101",
    date_to = "20120101",
    sampling_time = "1050",
    bathing_season_only = TRUE
  ))
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

# Rest -------------------------------------------------------------------------
if (FALSE)
{

  head(dates_available)

  length(urls)
  length(dates)

  head(substr(names(urls), 1, 8))
  head(sort(dates))

  for (date_range in date_ranges) {

    #date_range <- date_ranges[[1]]
    print(date_range)

    fhpredict::provide_rain_data_for_bathing_spot(
      user_id, spot_id,
      sampling_time = "1050",
      date_range = date_range
    )
  }

  diff(date_ranges[[1]])

  stopifnot(length(dates) == 6)

  # Get data in the format that is required by build_and_validate_model()
  spot_data <- fhpredict::provide_input_data(user_id, spot_id)

  # Remove empty data frames
  #spot_data <- spot_data[lengths(spot_data) > 0]

  #reset_time <- function(x) as.POSIXct(substr(as.character(x), 1, 10))

  #spot_data$hygiene$datum <- reset_time(spot_data$hygiene$datum)
  #spot_data$r$datum <- reset_time(spot_data$r$datum)

  result <- fhpredict:::build_and_validate_model(
    spot_data = spot_data,
    prefix = "spot18_"
  )

  fhpredict::api_add_model(user_id, spot_id, result$stanfit, comment = "great!")

  object.size(result[[3]])
  rstanarm::launch_shinystan(result[[3]])


  dn <- data.frame(r_mean_mean_23 = seq(0, 40, .5))

  pp <- apply(rstanarm::posterior_predict(result[[3]], newdata = dn), 2,
              quantile, probs = c(0.025, 0.5, 0.975))

  result[[3]]$model

  plot(dn$r_mean_mean_23, pp[2,])
  lines(dn$r_mean_mean_23, pp[1,])
  lines(dn$r_mean_mean_23, pp[3,])
}
