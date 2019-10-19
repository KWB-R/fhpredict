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
  file <- file.path(get_environment_var("TEMP"), ".postgres_api_token")

  read_token <- function() kwb.utils::catAndRun(
    get_text("reading_token", file = file),
    dbg = dbg,
    readLines(file)
  )

  write_token <- function(token) kwb.utils::catAndRun(
    get_text("writing_token", file = file),
    dbg = dbg,
    expr = {

      result <- try(writeLines(token, file))

      if (inherits(result, "try-error")) {

        info <- fs::file_info(dirname(file))

        clean_stop(get_text(
          "no_write_permission",
          file = file,
          folder = dirname(file),
          permissions = info$permissions,
          user = info$user,
          group = info$group,
          error = as.character(result)
        ))
      }
    }
  )

  new_token <- function() kwb.utils::catAndRun(
    get_text("requesting_token"),
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
