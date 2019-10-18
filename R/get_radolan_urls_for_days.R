# get_radolan_urls_for_days ----------------------------------------------------
# dates <- seq(as.Date("2018-07-01"), as.Date("2018-08-01"), by = 1)
# urls <- get_radolan_urls_for_days(dates)
get_radolan_urls_for_days <- function(dates, diff_days = 10, time = "1050")
{
  # Call fhpredict:::test_performance_get_radolan_urls() to find a good value
  # for diff_days

  stopifnot(! is.unsorted(dates))

  # Create date ranges from the vector of dates
  date_ranges <- group_dates_by_diff(dates, diff_days = diff_days)

  # Get URLs for each of the ranges
  urls_list <- get_radolan_urls_in_date_ranges(date_ranges, time = time)

  # Put all URLs into an atomic vector
  urls <- unlist(urls_list)

  # Return if there is not at least one URL
  if (length(urls) == 0) {

    return(character())
  }

  # Check if all URLs are unique
  stopifnot(all(! duplicated(urls)))

  # Convert URL names to dates
  url_dates <- extract_date_from_url_name(names(urls))

  # Check if all dates to which the URLs relate are unique
  stopifnot(all(! duplicated(url_dates)))

  # Return only the URLs related to dates that were requested
  urls[url_dates %in% dates]
}

# extract_date_from_url_name ---------------------------------------------------
extract_date_from_url_name <- function(name)
{
  stopifnot(all(grepl("^\\d{12}$", name)))

  as.Date(substr(name, 1, 8), format = "%Y%m%d")
}
