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
  date_end = lubridate::rollback(Sys.Date(), roll_to_first = TRUE)
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

  url_hist_file <- function(years, months, subdir, prefix, year, month = "01") {

    urls <- sprintf(
      paste0("%s/%s/", subdir, "-%s%s.tar.gz"),
      base_urls[[paste0(prefix, "_historical")]], years, years, months
    )

    indices <- seq.int(which(years == year & months == month), length(urls))

    urls[indices] <- urls[indices] %>%
      stringr::str_replace(paste0(subdir, "-"), subdir)

    urls
  }

  # Return download links for "hourly" and "daily" in a list
  list(
    daily_historical_urls = url_hist_file(
      years = year_string(date_start_daily, date_end),
      months = month_string(date_start_daily, date_end),
      subdir = "SF",
      prefix = "daily",
      year = "2009"
    ),
    hourly_historical_urls = url_hist_file(
      years = year_string(date_start_hourly, date_end),
      months = month_string(date_start_hourly, date_end),
      subdir = "RW",
      prefix = "hourly",
      year = "2006"
    )
  )
}

#' Download Radolan Files on DWD Server
#' @param temporal_resolution "daily" or "hourly" (default: daily)
#' @param export_dir export directory (default: "data" in current working
#' directory)
#' @importFrom magrittr %>%
#' @importFrom kwb.utils catAndRun
#' @importFrom fs dir_create
#' @importFrom utils download.file
#' @return list with "daily_historical_urls" and "hourly_historical_urls"
#' @export
download_radolan <- function(
  temporal_resolution = "daily", export_dir = "data"
) {

  download_daily_historical <- function(url) {

    daily_hist_dir <- sprintf("%s/daily/historical", export_dir)

    fs::dir_create(daily_hist_dir, recursive = TRUE)

    export_path <- sprintf("%s/%s", daily_hist_dir, basename(url))

    kwb.utils::catAndRun(
      messageText = sprintf(
        'Download: "daily, historical" and save to %s', export_path
      ),
      expr = try(
        download.file(url = url, destfile = export_path, mode = "wb")
      )
    )
  }

  download_hourly_historical <- function(url) {

    hourly_hist_dir <- sprintf("%s/hourly/historical", export_dir)

    fs::dir_create(hourly_hist_dir, recursive = TRUE)

    export_path <- sprintf("%s/%s", hourly_hist_dir, basename(url))

    kwb.utils::catAndRun(
      messageText = sprintf(
        'Download: "daily, historical" and save to %s', export_path
      ),
      expr = try(
        utils::download.file(url = url, destfile = export_path, mode = "wb")
      )
    )
  }

  urls <-  get_radolan_urls()

  message_text <- sprintf(
    "Download: '%s' historical radolan data", temporal_resolution
  )

  url_key <- paste0(temporal_resolution, "_historical_urls")

  resolution_to_function <- list(
    daily = download_daily_historical,
    hourly = download_hourly_historical
  )

  if (is.null(resolution_to_function[[temporal_resolution]])) {

    stop("temporal_resolution must be one of 'daily', 'hourly'", call. = FALSE)
  }

  kwb.utils::catAndRun(
    messageText = message_text,
    expr = sapply(urls[[url_key]], FUN = fun)
  )
}

#download_radolan(temporal_resolution = "daily")
