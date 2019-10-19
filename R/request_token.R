# request_token ----------------------------------------------------------------

#' Request a Token to Access the Flusshygiene Postgres API
#'
#' The function requires the following envirionment variables to be set:
#' \itemize{
#'   \item AUTH0_REQ_URL
#'   \item AUTH0_CLIENT_ID
#'   \item AUTH0_CLIENT_SECRET
#'   \item AUTH0_AUDIENCE
#' }
#'
#' @keywords internal
#'
request_token <- function()
{
  url <- get_environment_var("AUTH0_REQ_URL")

  config <- httr::add_headers("content-type" = "application/json")

  body <- as.character(jsonlite::toJSON(auto_unbox = TRUE, list(
    client_id = get_environment_var("AUTH0_CLIENT_ID"),
    client_secret = get_environment_var("AUTH0_CLIENT_SECRET"),
    audience = get_environment_var("AUTH0_AUDIENCE"),
    grant_type = "client_credentials"
  )))

  # Request a new token
  response <- httr::POST(url, config = config, body = body, encode = "json")

  # Get the status code (200 means success)
  status <- httr::status_code(response)

  if (status != 200) {
    message(get_text("request_token_failed", status = status))
    return(NULL)
  }

  httr::content(response)
}
