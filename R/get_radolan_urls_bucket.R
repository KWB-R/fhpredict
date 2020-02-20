# get_radolan_urls_bucket ------------------------------------------------------

#' Get URLs to Binary Radolan Files From Flusshygiene's Amazon Bucket
#'
#' This function requires two environment variables to be set:
#' "ENDPOINT_PROD": Endpoint to the service that returns URLs to Radolan
#' files, "TOKEN_PROD": token that is required to access the endpoint. Use
#' \code{usethis::edit_r_environ()} to open the .Renviron file in your home
#' directory and add two lines "ENDPOINT_PROD=..." and "TOKEN_PROD=..." where
#' ... is replaced with the corresponding values.
#'
#' @param from first day considered in format yyyymmdd, e.g. "20190625".
#'   Default: string representing "yesterday". If only the year and the month
#'   or only the year are given, the first day of the month and the first day
#'   of the year, respectively, are assumed.
#' @param to last day considered in format yyyymmdd, e.g. "20190625". Default:
#'   same day as given in \code{from}. If only the year and the month
#'   or only the year are given, the last day of the month and the last day
#'   of the year, respectively, are assumed.
#' @param time considered, given in format HHMM, e.g. "1050". Default: "", i.e.
#'   URLs are not filtered by time
#' @param bathing_season_only = FALSE if \code{TRUE} (the default is
#'   \code{FALSE}), only URLs related to days between May 1 and September 30 are
#'   returned
#' @return vector of character containing the URLs to the radolan files. The
#'   date and time to which the data in the files relate are encoded in the
#'   names of the returned vector elements in the format "yyyymmddHHMM".
#' @export
#' @examples
#' \dontrun{
#' # Get URLs to available files for 2019-06-25, 10:50
#' urls <- get_radolan_urls_bucket(from = "20190625", time = "1050")
#'
#' # Read the first file (there should be only one such file!)
#' raster_rain <- kwb.dwd::read_binary_radolan_file(urls[1])
#' }
#'
get_radolan_urls_bucket <- function(
  from = format(Sys.Date() - 1, "%Y%m%d"), to = from, time = "",
  bathing_season_only = FALSE
)
{
  #kwb.utils::assignPackageObjects("fhpredict")
  #endpoint <- get_environment_var("ENDPOINT_PROD")
  #token <- get_environment_var("TOKEN_PROD")
  endpoint <- get_environment_var("FHPREDICT_RADOLAN_API_URL_PROD")
  token <- get_environment_var("FHPREDICT_RADOLAN_API_TOKEN_PROD")

  request <- sprintf(
    "%s?from=%s&to=%s&time=%s",
    endpoint,
    partial_date_string_to_date_string(as.character(from)),
    partial_date_string_to_date_string(as.character(to), to_first = FALSE),
    time
  )

  response <- httr::GET(request, httr::add_headers("x-api-key" = token))

  content <- httr::content(response, "parsed")

  if (httr::http_error(response) || is.null(content$files)) {

    clean_stop(paste(collapse = "\n", c(
      "Could not get URLs to Radolan files.",
      httr::http_status(response)$message,
      content$error$message,
      content$message
    )))
  }

  urls <- sapply(content$files, "[[", "url")

  if (length(urls) == 0) {
    return(character())
  }

  start <- nchar("raa01-sf_10000-")
  mmdd <- substr(basename(urls), start + 3, start + 6)

  if (bathing_season_only) {
    urls <- urls[mmdd > "0430" & mmdd < "1001"]
  }

  if (length(urls) == 0) {
    return(character())
  }

  date_strings <- kwb.utils::extractSubstring("-(\\d{10})-", basename(urls), 1)

  stats::setNames(urls, paste0("20", date_strings))
}

# is_in_bathing_season ---------------------------------------------------------
is_in_bathing_season <- function(dates)
{
  stopifnot(inherits(dates, "Date"))

  month_day_strings <- format(dates, "%m%d")

  month_day_strings > "0430" & month_day_strings < "1001"
}

# partial_date_string_to_date_string -------------------------------------------
partial_date_string_to_date_string <- function(x, to_first = TRUE)
{
  stopifnot(is.character(x))
  stopifnot(length(x) == 1)

  # Nothing to do if this already looks like yyyymmdd
  if (grepl("^\\d{8}$", x)) {
    return(x)
  }

  suffix <- if (grepl("^\\d{6}$", x)) {
    ifelse(to_first, "01", last_day_of_yyyymm(x))
  } else if (grepl("^\\d{4}$", x)) {
    ifelse(to_first, "0101", "1231")
  } else {
    clean_stop("Unexpected input to partial_date_string_to_date_string(): ", x)
  }

  paste0(x, suffix)
}

# last_day_of_yyyymm -----------------------------------------------------------
last_day_of_yyyymm <- function(x)
{
  stopifnot(is.character(x))
  stopifnot(length(x) == 1)
  stopifnot(grepl("^\\d{6}$", x))

  year <- as.integer(substr(x, 1, 4))
  month <- as.integer(substr(x, 5, 6))

  if (month == 12) {
    31
  } else {
    substr(as.Date(sprintf("%4d-%2d-01", year, month + 1)) - 1, 9, 10)
  }
}
