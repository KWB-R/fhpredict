# postgres_get -----------------------------------------------------------------

#' Send GET Request to Postgres API
#'
#' @param path relative path, e.g. \code{bathingspots/1} to get data for
#'   bathingspot with ID 1
#' @return In case of success this function returns what
#'   \code{httr::content(response, as = "parsed")} returns. In case of failure
#'   an empty list with attribute \code{response} (containing the response
#'   object returned by \code{\link[httr]{GET}}) is returned.
#' @export
#'
postgres_get <- function(path)
{
  postgres_request(path, "GET")
}

# postgres_post ----------------------------------------------------------------

#' Send POST Request to Postgres API
#'
#' @param path relative path, e.g. \code{users/3/bathingspots/18/genericInputs}
#'   to create a generic input for bathing spot of user 3 with ID 18
#' @param body list with the fields to be set as \code{key = value} pairs, e.g.
#'   \code{list(name = "lirum larum")}
#' @return In case of success this function returns what
#'   \code{httr::content(response, as = "parsed")} returns. In case of failure
#'   an empty list with attribute \code{response} (containing the response
#'   object returned by \code{\link[httr]{POST}}) is returned.
#' @export
#'
postgres_post <- function(path, body = NULL)
{
  postgres_request(path, "POST", body)
}

# postgres_delete --------------------------------------------------------------

#' Send DELETE Request to Postgres API
#'
#' @param path relative path, e.g. \code{users/3/bathingspots/18/genericInputs}
#'   to delete a generic input for bathing spot with id 18 of user with id 3
#' @return In case of success this function returns what
#'   \code{httr::content(response, as = "parsed")} returns. In case of failure
#'   an empty list with attribute \code{response} (containing the response
#'   object returned by \code{\link[httr]{DELETE}}) is returned.
#' @export
#'
postgres_delete <- function(path)
{
  postgres_request(path, "DELETE")
}

# postgres_request -------------------------------------------------------------
postgres_request <- function(path, type = "GET", body = NULL)
{
  stopifnot(type %in% c("GET", "POST", "DELETE"))

  url <- paste0(assert_final_slash(get_environment_var("API_URL")), path)

  token <- get_postgres_api_token()

  config <- httr::add_headers("Authorization" = paste("Bearer", token))

  response <- if (type == "GET") {

    httr::GET(url, config = config)

  } else if (type == "DELETE") {

    httr::DELETE(url, config = config)

  } else if (type == "POST") {

    httr::POST(url, config = config, body = body, encode = "json")
  }

  status <- httr::http_status(response)

  if (status$category != "Success") {

    message(
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

# safe_postgres_get ------------------------------------------------------------
safe_postgres_get <- function(path)
{
  result <- postgres_get(path)
  stop_on_request_failure(result)
  result
}

# safe_postgres_post -----------------------------------------------------------
safe_postgres_post <- function(path, body)
{
  result <- postgres_post(path, body)
  stop_on_request_failure(result)
  result
}

# stop_on_request_failure ------------------------------------------------------
stop_on_request_failure <- function(result)
{
  response <- attr(result, "response")

  if (is.null(response)) {
    return()
  }

  status <- httr::http_status(response)

  if (status$category != "Success") {
    clean_stop("HTTP request failed: ", status$message)
  }
}
