#' Get URLs to Available Radolan Files on DWD Server
#'
#' @param date_start_daily date string (yyyy-mm-dd) of first day of daily
#'   records. Default: "2006-10-01"
#' @param date_start_hourly date string (yyyy-mm-dd) of first day of hourly
#'   records. Default: "2005-06-01"
#' @param date_end_daily date string (yyyy-mm-dd) of last day of daily
#'   records. Defaults to the first day of the current month.
#' @param date_end_hourly date string (yyyy-mm-dd) of last day of hourly
#'   records. Defaults to \code{date_end_daily}.
#' @importFrom magrittr %>%
#' @importFrom kwb.utils resolve
#' @importFrom lubridate rollback
#' @importFrom stringr str_replace
#' @importFrom fs dir_create
#' @return list with "daily_historical_urls" and "hourly_historical_urls"
#' @export
get_radolan_urls <- function(
  date_start_daily = "2006-10-01", date_start_hourly = "2005-06-01",
  date_end_daily = lubridate::rollback(Sys.Date(), roll_to_first = TRUE),
  date_end_hourly = date_end_daily
) {

  base_urls <- kwb.utils::resolve(list(
    base_url = "ftp://ftp-cdc.dwd.de/pub/CDC/grids_germany",
    daily_radolan = "<base_url>/daily/radolan",
    hourly_radolan = "<base_url>/hourly/radolan",
    daily_historical = "<daily_radolan>/historical",
    daily_recent = "<daily_radolan>/recent",
    hourly_historical = "<hourly_radolan>/historical/bin",
    hourly_recent = "<hourly_radolan>/recent/bin"
  ))

  # Define helper functions
  date_string <- function(date_start, date_end, format) {
    format(format = format, seq(
      lubridate::ymd(date_start),
      lubridate::ymd(date_end),
      by = 'months'
    ))
  }

  year_string <- function(start, end) date_string(start, end, format = "%Y")
  month_string <- function(start, end) date_string(start, end, format = "%m")

  url_hist_file <- function(
    years, months, subdir, resolution, year, month = "01"
  ) {

    urls <- sprintf(
      paste0("%s/%s/", subdir, "-%s%s.tar.gz"),
      base_urls[[paste0(resolution, "_historical")]], years, years, months
    )

    indices <- seq.int(which(years == year & months == month), length(urls))

    urls[indices] <- urls[indices] %>%
      stringr::str_replace(paste0(subdir, "-"), subdir)

    urls
  }

  # Return download links for "hourly" and "daily" in a list
  list(
    daily_historical_urls = url_hist_file(
      years = year_string(date_start_daily, date_end_daily),
      months = month_string(date_start_daily, date_end_daily),
      subdir = "SF",
      resolution = "daily",
      year = "2009"
    ),
    hourly_historical_urls = url_hist_file(
      years = year_string(date_start_hourly, date_end_hourly),
      months = month_string(date_start_hourly, date_end_hourly),
      subdir = "RW",
      resolution = "hourly",
      year = "2006"
    )
  )
}
