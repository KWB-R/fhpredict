# is_valid_postgres_api_token --------------------------------------------------
is_valid_postgres_api_token <- function(token)
{
  response <- httr::GET(
    url = get_environment_var("API_URL"),
    config = httr::add_headers(Authorization = paste("Bearer", token))
  )

  stop_on_request_failure(response = response)

  success <- httr::content(response, "parsed", encoding = "UTF-8")$success

  success <- kwb.utils::defaultIfNULL(success, FALSE)

  if (! success) {
    message(get_text("invalid_token"))
  }

  success
}
