# postgres_get -----------------------------------------------------------------

#' Send GET Request to Postgres API
#'
#' @param path relative path, e.g. \code{bathingspots/1} to get data for
#'   bathingspot with ID 1
#' @param \dots further arguments passed to \code{fhpredict:::postgres_request}
#' @return In case of success this function returns what
#'   \code{httr::content(response, as = "parsed")} returns. In case of failure
#'   an empty list with attribute \code{response} (containing the response
#'   object returned by \code{\link[httr]{GET}}) is returned.
#' @export
#'
postgres_get <- function(path, ...)
{
  postgres_request(path, "GET", ...)
}

# postgres_post ----------------------------------------------------------------

#' Send POST Request to Postgres API
#'
#' @param path relative path, e.g. \code{users/3/bathingspots/18/genericInputs}
#'   to create a generic input for bathing spot of user 3 with ID 18
#' @param body list with the fields to be set as \code{key = value} pairs, e.g.
#'   \code{list(name = "lirum larum")}
#' @param encode passed to \code{\link[httr]{POST}}. Default: "json". Set to
#'   "multipart" for uploading files!
#' @param \dots further arguments passed to \code{fhpredict:::postgres_request}
#' @return In case of success this function returns what
#'   \code{httr::content(response, as = "parsed")} returns. In case of failure
#'   an empty list with attribute \code{response} (containing the response
#'   object returned by \code{\link[httr]{POST}}) is returned.
#' @export
#'
postgres_post <- function(path, body = NULL, encode = "json", ...)
{
  postgres_request(path, "POST", body = body, encode = encode, ...)
}

# postgres_delete --------------------------------------------------------------

#' Send DELETE Request to Postgres API
#'
#' @param path relative path, e.g. \code{users/3/bathingspots/18/genericInputs}
#'   to delete a generic input for bathing spot with id 18 of user with id 3
#' @param \dots further arguments passed to \code{fhpredict:::postgres_request}
#' @return In case of success this function returns what
#'   \code{httr::content(response, as = "parsed")} returns. In case of failure
#'   an empty list with attribute \code{response} (containing the response
#'   object returned by \code{\link[httr]{DELETE}}) is returned.
#' @export
#'
postgres_delete <- function(path, ...)
{
  postgres_request(path, "DELETE", ...)
}

# postgres_put -----------------------------------------------------------------

# Send PUT Request to Postgres API
postgres_put <- function(path, body = NULL, encode = "json", ...)
{
  postgres_request(path, "PUT", body = body, encode = encode, ...)
}

# postgres_request -------------------------------------------------------------

#' Do a Request to the Flusshygiene Postgres API
#'
#' @param path relative path to endpoint
#' @param type one of "GET", "POST", "DELETE"
#' @param \dots further arguments passed to one of \code{\link[httr]{GET}},
#'   \code{\link[httr]{DELETE}}, \code{\link[httr]{POST}}, depending on
#'   \code{type}
#' @param token token required to authenticate for the API. If omitted, a token
#'   will be provided by a call to \code{fhpredict:::get_postgres_api_token}.
#' @param config optional. Configuration created with
#'   \code{\link[httr]{add_headers}}
#' @param verbose if \code{TRUE} (the default is \code{FALSE}), the result of
#'   the request will be printed
#' @keywords internal
postgres_request <- function(
  path, type = "GET", ..., token = NULL, config = NULL, verbose = FALSE
)
{
  #kwb.utils::assignPackageObjects("fhpredict")

  # Select function from httr corresponding to the given type
  httr_function <- kwb.utils::selectElements(type, x = list(
    GET = httr::GET,
    POST = httr::POST,
    DELETE = httr::DELETE,
    PUT = httr::PUT
  ))

  if (is.null(config)) {

    token <- kwb.utils::defaultIfNULL(token, get_postgres_api_token())

    config <- get_httr_config_with_token(token)
  }

  response <- httr_function(url = path_to_api_url(path), config = config, ...)

  #Call without "..." for debugging:
  #response <- httr_function(url, config = config)

  status <- httr::http_status(response)

  if (status$category != "Success") {

    if (verbose) message(
      sprintf("%s request '%s' returned with error:\n", type, path),
      sprintf("- status code: %d\n", httr::status_code(response)),
      sprintf("- category: %s\n", status$category),
      sprintf("- reason: %s\n", status$reason),
      sprintf("- message: %s\n", status$message)
    )

    return(structure(list(), response = response))
  }

  httr::content(response, as = "parsed")
}

# path_to_api_url --------------------------------------------------------------
#' @importFrom kwb.utils assertFinalSlash
path_to_api_url <- function(path = "")
{
  base_url <- get_environment_var("FHPREDICT_PG_API_URL")
  paste0(kwb.utils::assertFinalSlash(base_url), path)
}

# get_httr_config_with_token ---------------------------------------------------
get_httr_config_with_token <- function(token)
{
  httr::add_headers("Authorization" = paste("Bearer", token))
}
