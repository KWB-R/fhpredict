# available_radolan_times_of_day -----------------------------------------------
available_radolan_times_of_day <- function(date = Sys.Date())
{
  if (! inherits(date, "Date")) {

    date <- try(as.Date(date), silent = TRUE)

    if (inherits(date, "try-error")) {
      clean_stop("date must be a Date object or a 'yyyy-mm-dd' string")
    }
  }

  if (length(date) > 1L) {
    return(lapply(stats::setNames(nm = date), available_radolan_times_of_day))
  }

  get_time <- function(x) substr(x, 9, 12)

  date_string <- function(x) format.Date(x, "%Y%m%d")

  urls <- get_radolan_urls_bucket(date_string(date))

  if (length(urls) == 0L) {
    clean_stop(sprintf("No RADOLAN files available for day '%s'", date))
  }

  get_time(names(urls))
}
