# get_radolan_urls_bucket ------------------------------------------------------

#' Get URLs to Binary Radolan Filed From Flusshygiene's Amazon Bucket
#'
#' This function requires an environment variable "TOKEN_PROD" to be set to the
#' token that is required to access the "Production Endpoint". Use
#' \code{usethis::edit_r_environ()} to open the .Renviron file in your home
#' directory and add a line "TOKEN_PROD=..." where ... is replaced with the
#' token.
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
  token <- Sys.getenv("TOKEN_PROD")

  if (token == "") {
    stop("Set the token in environment variable 'TOKEN_PROD'", call. = FALSE)
  }

  endpoint <- "https://bvq0pskv98.execute-api.eu-central-1.amazonaws.com/prod"

  url <- sprintf("%s?from=%s&to=%s&time=%s", endpoint, from, to, time)

  response <- httr::GET(url, httr::add_headers("x-api-key" = token))

  sapply(httr::content(response, "parsed")$files, "[[", "url")
}
