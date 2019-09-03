# get_postgres_api_token -------------------------------------------------------

#' Get a Valid Token to Access the Postgres API
#'
#' If a token is stored locally it is checked whether this token is valid. If
#' the token is valid, it is returned. If the token is not valid or if there is
#' no token stored locally, request a new token, store it locally and return it.
#'
#' @param dbg if \code{TRUE}, debug messages are shown
#' @export
#'
get_postgres_api_token <- function(dbg = FALSE)
{
  extdata_path <- system.file("extdata", package = "fhpredict")

  file <- file.path(extdata_path, ".postgres_api_token")

  read_token <- function() kwb.utils::catAndRun(
    sprintf("Reading access token from '%s'", file),
    dbg = dbg,
    readLines(file)
  )

  write_token <- function(token) kwb.utils::catAndRun(
    sprintf("Writing access token to '%s'", file),
    dbg = dbg,
    expr = {

      result <- try(writeLines(token, file))

      if (inherits(result, "try-error")) {

        info <- fs::file_info(dirname(file))

        clean_stop(
          "No permission to write to file ", file, "\n",
          "Permissions/user/group of folder ", dirname(file), ":\n",
          info$permissions, "/", info$user, "/", info$group, "\n",
          "Error message: ", as.character(result)
        )
      }
    }
  )

  new_token <- function() kwb.utils::catAndRun(
    "Requesting a new access token",
    dbg = dbg,
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
