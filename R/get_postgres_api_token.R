# get_postgres_api_token -------------------------------------------------------
get_postgres_api_token <- function()
{
  file <- "~/.postgres_api_token"

  if (file.exists(file)) {

    token <- kwb.utils::catAndRun(
      sprintf("Reading access token from '%s'", file),
      readLines(file)
    )

  } else {

    token <- kwb.utils::catAndRun(
      "Requesting a new access token", {
        if (! is.null(token_data <- request_token())) {
          kwb.utils::selectElements(token_data, "access_token")
        }
      })

    if (! is.null(token)) kwb.utils::catAndRun(
      sprintf("Writing access token to '%s'", file),
      writeLines(token, file)
    )
  }

  token
}
