# is_valid_postgres_api_token --------------------------------------------------
is_valid_postgres_api_token <- function(token)
{
  url <- get_environment_var("API_URL")

  config <- httr::add_headers(Authorization = paste("Bearer", token))

  response <- httr::GET(url, config = config)

  success <- httr::content(response, "parsed")$success
  success <- kwb.utils::defaultIfNULL(success, FALSE)

  if (! success) {
    message(get_text("invalid_token"))
    utils::str(httr::headers(response))
  }

  success
}
