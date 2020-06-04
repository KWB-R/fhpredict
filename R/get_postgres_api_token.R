# get_postgres_api_token -------------------------------------------------------

#' Get a Valid Token to Access the Postgres API
#'
#' If a token is stored locally it is checked whether this token is valid. If
#' the token is valid, it is returned. If the token is not valid or if there is
#' no token stored locally, request a new token, store it locally and return it.
#'
#' @param dbg if \code{TRUE}, debug messages are shown
#' @keywords internal
#'
get_postgres_api_token <- function(dbg = TRUE)
{
  # Full path to a file in the TEMP folder that stores the token
  file <- token_file()

  # If the token file exists, read the token from the file. Otherwise token will
  # be NULL
  token <- if (file.exists(file)) {
    read_token(file, dbg = dbg)
  }

  # If there was no file or if the token that was read from the file is not
  # valid we request for a new token and save it in the file
  if (is.null(token) || ! is_valid_postgres_api_token(token)) {

    token <- kwb.utils::catAndRun(

      get_text("requesting_token"),
      dbg = dbg,
      newLine = 1,
      expr = {

        if (is.null(token_data <- request_token())) {
          clean_stop("Requesting the token from auth0 failed!")
        }

        token <- kwb.utils::selectElements(token_data, "access_token")

        write_token(token, file, dbg = dbg)

        token
      }
    )
  }

  # We expect the new token to be valid!
  if (! (is_valid <- is_valid_postgres_api_token(token))) {

    clean_stop(kwb.utils::getAttribute(is_valid, "error"))
  }

  token
}

# token_file -------------------------------------------------------------------
token_file <- function()
{
  file.path(get_environment_var("TEMP"), ".postgres_api_token")
}

# read_token -------------------------------------------------------------------
read_token <- function(file = token_file(), dbg = TRUE)
{
  kwb.utils::catAndRun(
    get_text("reading_token", file = file),
    dbg = dbg,
    readLines(file)
  )
}

# write_token ------------------------------------------------------------------
write_token <- function(token, file = token_file(), dbg = TRUE)
{
  kwb.utils::catAndRun(
    get_text("writing_token", file = file),
    dbg = dbg,
    expr = {

      result <- try(writeLines(token, file))

      if (is_error(result)) {

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
}
