# test_performance_get_radolan_urls --------------------------------------------
test_performance_get_radolan_urls <- function(dates)
{
  stopifnot(inherits(dates, "Date"))

  # Vector of all different gap sizes to test for
  gaps <- seq_len(as.integer(diff(range(dates))))

  message("Testing ", length(gaps), " different gap sizes to create ranges")

  # Build groups of consecutive dates with a maximum gap within each group
  date_ranges_list <- lapply(
    X = gaps, FUN = group_dates_by_diff, dates = dates
  )

  n_rows <- sapply(date_ranges_list, nrow)

  indices_change <- sapply(unique(n_rows), function(n) which(n_rows == n)[1])

  # Get URLs by calling get_radolan_urls_bucket() a different number of times
  runtimes <- lapply(date_ranges_list[indices_change], function(date_ranges) {
    message("Getting URLs for ", nrow(date_ranges), " date ranges")
    system.time(get_radolan_urls_in_date_ranges(date_ranges))
  })

  data <- data.frame(
    diff_days = gaps[indices_change],
    elapsed = sapply(runtimes, "[", "elapsed")
  )

  graphics::barplot(
    data$elapsed,
    names.arg = data$diff_days,
    xlab = "Max. date difference within range (in days)",
    ylab = "Time to get URLs (in seconds)"
  )

  data
}
