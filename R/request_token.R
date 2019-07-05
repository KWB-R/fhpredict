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

  #  When your token expires you can do this request
  response <- httr::POST(url, config = config, body = body, encode = "json")

  status <- httr::status_code(response) # should be 200

  if (status != 200) {
    message(sprintf(
      "Request for token failed. Status: %d. Returning NULL.", status
    ))
    return(NULL)
  }

  httr::content(response)
}
