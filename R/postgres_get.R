# postgres_get -----------------------------------------------------------------

#' Send GET Request to Postgres API
#'
#' @param path relative path, e.g. \code{bathingspots/1} to get data for
#'   bathingspot with ID 1
#' @return In case of success this function returns what
#'   \code{httr::content(response, "parsed")} returns. In case of failure an
#'   empty list with attribute \code{response} (containing the response object
#'   returned by \code{\link[httr]{GET}}) is returned.
#' @export
#'
postgres_get <- function(path)
{
  url <- paste0(assert_final_slash(get_environment_var("API_URL")), path)

  token <- get_postgres_api_token()

  config <- httr::add_headers("Authorization" = paste("Bearer", token))

  response <- httr::GET(url, config = config)

  parsed <- httr::content(response, "parsed")

  status <- httr::status_code(response)

  if (status != 200) {
    error <- kwb.utils::defaultIfNULL(parsed$message, "")
    message(sprintf("Get request '%s' returned with error:\n'%s'", path, error))
    return(structure(list(), response = response))
  }

  parsed
}

