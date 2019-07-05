# get_postgres_api_token -------------------------------------------------------
get_postgres_api_token <- function()
{
  file <- "~/.postgres_api_token"

  read_token <- function() kwb.utils::catAndRun(
    sprintf("Reading access token from '%s'", file),
    readLines(file)
  )

  write_token <- function(token) kwb.utils::catAndRun(
    sprintf("Writing access token to '%s'", file),
    writeLines(token, file)
  )

  new_token <- function() kwb.utils::catAndRun(
    "Requesting a new access token",
    if (! is.null(token_data <- request_token())) {
      kwb.utils::selectElements(token_data, "access_token")
    }
  )

  # If a token is stored and if it is valid, return the stored token
  if (file.exists(file) &&
      is_valid_postgres_api_token(token <- read_token())) {
    return(token)
  }

  # If we arrive here, there is no stored token or the stored token is not valid
  if (! is.null(token <- new_token())) {
    write_token(token)
  }

  token
}
