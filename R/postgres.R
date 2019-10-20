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
#' @param verbose if \code{TRUE} (the default is \code{FALSE}), the result of
#'   the request will be printed
#' @keywords internal
postgres_request <- function(
  path, type = "GET", ..., token = NULL, verbose = FALSE
)
{
  stopifnot(type %in% c("GET", "POST", "DELETE"))

  if (is.null(token)) {
    token <- get_postgres_api_token()
  }

  url <- paste0(assert_final_slash(get_environment_var("API_URL")), path)

  config <- httr::add_headers("Authorization" = paste("Bearer", token))

  response <- if (type == "GET") {

    httr::GET(url, config = config, ...)

  } else if (type == "DELETE") {

    httr::DELETE(url, config = config, ...)

  } else if (type == "POST") {

    httr::POST(url, config = config, ...)
  }

  status <- httr::http_status(response)

  if (status$category != "Success") {

    if (verbose) {
      message(
        sprintf("%s request '%s' returned with error:\n", type, path),
        sprintf("- status code: %d\n", httr::status_code(response)),
        sprintf("- category: %s\n", status$category),
        sprintf("- reason: %s\n", status$reason),
        sprintf("- message: %s\n", status$message)
      )
    }

    return(structure(list(), response = response))
  }

  httr::content(response, as = "parsed")
}
