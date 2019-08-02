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
#'   \code{httr::content(response, "parsed")$data} returns. In case of failure
#'   an empty list with attribute \code{response} (containing the response
#'   object returned by \code{\link[httr]{POST}}) is returned.
#' @export
#'
postgres_post <- function(path, body = NULL)
{
  postgres_request(path, "POST", body)
}

# postgres_request -------------------------------------------------------------
postgres_request <- function(path, type = "GET", body = NULL)
{
  stopifnot(type %in% c("GET", "POST"))

  url <- paste0(assert_final_slash(get_environment_var("API_URL")), path)

  token <- get_postgres_api_token()

  config <- httr::add_headers("Authorization" = paste("Bearer", token))

  response <- if (type == "GET") {

    httr::GET(url, config = config)

  } else if (type == "POST") {

    httr::POST(url, config = config, body = body, encode = "json")
  }

  parsed <- httr::content(response, "parsed")

  status <- httr::status_code(response)

  if (type == "GET" && status != 200) {
    error <- kwb.utils::defaultIfNULL(parsed$message, "")
    message(sprintf("GET request '%s' returned with error:\n'%s'", path, error))
    return(structure(list(), response = response))
  }

  if (type == "POST" && status != 201) {
    error <- kwb.utils::defaultIfNULL(parsed$message, "")
    message(sprintf("POST request '%s' returned with error:\n'%s'", path, error))
    return(structure(list(), response = response))
  }

  if (type == "GET") {

    parsed

  } else {

    parsed$data
  }
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
  get_slot <- function(x) kwb.utils::selectElements(result, x)

  if (! get_slot("success")) {
    clean_stop("HTTP request failed: ", get_slot("message"))
  }
}
