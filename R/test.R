#' Get URLs to Available Radolan Files on DWD Server
#'
#' @export
get_radolan_urls <- function() {

  `%>%` <- magrittr::`%>%`

  base_urls <- kwb.utils::resolve(list(
    base_url = "ftp://ftp-cdc.dwd.de/pub/CDC/grids_germany",
    daily_historical = "<base_url>/daily/radolan/historical",
    daily_recent = "<base_url>/daily/radolan/recent/bin",
    hourly_historical = "<base_url>/hourly/radolan/historical/bin",
    hourly_recent = "<base_url>/hourly/radolan/historical/bin"
  ))

  ###############################################################################
  #### Daily
  ###############################################################################

  daily_date_start <- "2006-10-01"
  daily_date_end <- lubridate::rollback(Sys.Date(), roll_to_first = TRUE)

  daily_year <- format(format = "%Y", seq(
    lubridate::ymd(daily_date_start),
    lubridate::ymd(daily_date_end),
    by = 'months'
  ))

  daily_month <- format(format = "%m", seq(
    lubridate::ymd(daily_date_start),
    lubridate::ymd(daily_date_end),
    by = 'months'
  ))

  ###############################################################################
  #### Hourly
  ###############################################################################

  hourly_date_start <- "2005-06-01"
  hourly_date_end <-  lubridate::rollback(Sys.Date(), roll_to_first = TRUE)

  hourly_year <- format(format = "%Y", seq(
    lubridate::ymd(hourly_date_start),
    lubridate::ymd(hourly_date_end),
    by = 'months'
  ))

  hourly_month <- format(format = "%m", seq(
    lubridate::ymd(hourly_date_start),
    lubridate::ymd(hourly_date_end),
    by = 'months'
  ))

  ###############################################################################
  #### Saving download links for "hourly" and "daily" in list
  ###############################################################################

  url_daily_hist_file <- function() {

    tmp_daily_hist <- sprintf(
      "%s/%s/SF-%s%s.tar.gz",
      base_urls$daily_historical,
      daily_year,
      daily_year,
      daily_month
    )

    idx_start <- which(daily_year == "2009" & daily_month == "01")
    idx_end <- length(tmp_daily_hist)

    tmp_daily_hist[idx_start:idx_end] <- tmp_daily_hist[idx_start:idx_end] %>%
      stringr::str_replace("SF-", "SF")

    tmp_daily_hist
  }

  url_hourly_hist_file <- function() {

    tmp_hourly_hist <- sprintf(
      "%s/%s/RW-%s%s.tar.gz",
      base_urls$hourly_historical,
      hourly_year,
      hourly_year,
      hourly_month
    )

    idx_start <- which(hourly_year == "2006" & hourly_month == "01")
    idx_end <- length(tmp_hourly_hist)

    tmp_hourly_hist[idx_start:idx_end] <- tmp_hourly_hist[idx_start:idx_end] %>%
      stringr::str_replace("RW-", "RW")

    tmp_hourly_hist
  }

  list(
    daily_historical_urls = url_daily_hist_file(),
    hourly_historical_urls = url_hourly_hist_file()
  )
}

download_radolan <- function(
  temporal_resolution = "daily",
  export_dir = "data"
) {

  urls <-  get_radolan_urls()

  if (temporal_resolution == "daily") {

    kwb.utils::catAndRun(
      messageText = "Download: 'daily' historical radolan data",
      expr = {
        sapply(urls$daily_historical_urls, FUN = function(url) {

          daily_hist_dir <- sprintf("%s/daily/historical", export_dir)

          fs::dir_create(daily_hist_dir, recursive = TRUE)

          export_path <- sprintf("%s/%s", daily_hist_dir, basename(url))

          msg <- sprintf('Download: "daily, historical" and save to %s',
                         export_path)

          kwb.utils::catAndRun(messageText = msg, expr = {
            try(download.file(url = url, destfile = export_path, mode = "wb"))
          })
        })
      }
    )

  } else if (temporal_resolution == "hourly") {

    kwb.utils::catAndRun(
      messageText = "Download: 'hourly' historical radolan data",
      expr = {
        sapply(urls$hourly_historical_urls, FUN = function(url) {

          hourly_hist_dir <- sprintf("%s/hourly/historical", export_dir)

          fs::dir_create(hourly_hist_dir, recursive = TRUE)

          export_path <- sprintf("%s/%s", daily_hist_dir, basename(url))

          msg <- sprintf('Download: "daily, historical" and save to %s',
                         export_path)

          kwb.utils::catAndRun(
            messageText = msg, expr = {
              try(download.file(url = url, destfile = export_path, mode = "wb"))
            }
          )
        })
      }
    )

  } else {

    stop("temporal_resolution must be one of 'daily', 'hourly'", call. = FALSE)
  }
}

#download_radolan(temporal_resolution = "daily")
