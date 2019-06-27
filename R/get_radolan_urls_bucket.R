# get_radolan_urls_bucket ------------------------------------------------------

#' Get URLs to Binary Radolan Filed From Flusshygiene's Amazon Bucket
#'
#' This function requires two environment variables to be set:
#' "ENDPOINT_PROD": Endpoint to the service that returns URLs to Radolan
#' files, "TOKEN_PROD": token that is required to access the endpoint. Use
#' \code{usethis::edit_r_environ()} to open the .Renviron file in your home
#' directory and add two lines "ENDPOINT_PROD=..." and "TOKEN_PROD=..." where
#' ... is replaced with the corresponding values.
#'
#' @param from first day considered in format yyyymmdd, e.g. "20190625".
#'   Default: string representing "yesterday"
#' @param to last day considered in format yyyymmdd, e.g. "20190625". Default:
#'   same day as given in \code{from}
#' @param time considered, given in format HHMM, e.g. "1050". Default: "", i.e.
#'   URLs are not filtered by time
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
  from = format(Sys.Date() - 1, "%Y%m%d"), to = from, time = ""
)
{
  endpoint <- get_environment_var("ENDPOINT_PROD")
  token <- get_environment_var("TOKEN_PROD")

  url <- sprintf("%s?from=%s&to=%s&time=%s", endpoint, from, to, time)

  response <- httr::GET(url, httr::add_headers("x-api-key" = token))

  sapply(httr::content(response, "parsed")$files, "[[", "url")
}

# get_environment_var ----------------------------------------------------------
get_environment_var <- function(name)
{
  value <- Sys.getenv(name)

  if (! nzchar(value)) {
    stop(call. = FALSE, sprintf(
      "Please set the environment variable '%s'", name
    ))
  }

  name
}
