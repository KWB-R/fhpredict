# is_valid_postgres_api_token --------------------------------------------------
is_valid_postgres_api_token <- function(token)
{
  content <- safe_postgres_get(path = "", token = token)

  success <- kwb.utils::defaultIfNULL(content$success, FALSE)

  if (! success) {
    message(get_text("invalid_token"))
  }

  success
}
