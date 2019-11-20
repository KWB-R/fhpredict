# is_valid_postgres_api_token --------------------------------------------------
is_valid_postgres_api_token <- function(token)
{
  result <- try(safe_postgres_get("", token = token), silent = TRUE)

  if (! inherits(result, "try-error")) {
    return(TRUE)
  }

  structure(FALSE, error = paste(
    get_text("invalid_token"), as.character(result)
  ))
}
