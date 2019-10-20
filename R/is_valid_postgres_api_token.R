# is_valid_postgres_api_token --------------------------------------------------
is_valid_postgres_api_token <- function(token)
{
  content <- postgres_get(path = "", token = token)

  if (! is.null(response <- attr(content, "response"))) {

    status_code <- httr::status_code(response)

    # Expected status code for lacking authorisation
    if (status_code == 401) {
      return(FALSE)
    }

    # Unexpected request failure
    stop_on_request_failure(response = response)
  }

  success <- kwb.utils::defaultIfNULL(content$success, FALSE)

  if (! success) {
    message(get_text("invalid_token"))
  }

  success
}
