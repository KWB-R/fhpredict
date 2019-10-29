# api_get_timeseries -----------------------------------------------------------

#' Get Timeseries Data from the Postgres Database Via API
#'
#' This function calls \code{\link{postgres_get}} to get data from the endpoint
#' given by \code{path} and calls \code{fhpredict:::flatten_recursive_list} to
#' convert the list structure into a data frame. The time columns "createdAt"
#' and "updatedAt" are converted from a text timestamp given in ISO 8601 format
#' (e.g. "2019-10-22T23:38:29.003Z") to POSIXct objects in time zone
#' "Europe/Berlin". A new POSIXct column \code{dateTime} is created from the
#' original text columns \code{date} and \code{dateTime} as they are returned by
#' the API:
#' \itemize{
#'   \item{\code{(original) date} column: date and time information as text,
#'   e.g. "2019-09-21T00:00:00.000Z"}
#'   \item{\code{(original) dateTime} column: only time information as text,
#'   e.g. "10:50:00"}
#'   \item{\code{(new) dateTime} column: date and time as POSIXct in time zone
#'   "Europe/Berlin"}
#' }
#'
#' @param path (relative) path to API endpoint
#' @param subject name of data subject, to be used in messages
#' @param sort if \code{TRUE} (the default), the returned data frame will be
#'   sorted by the "dateTime" column
#' @param token passed to \code{fhpredict:::postgres_request}
#' @param type optional. If given, columns are removed and reordered using
#'   \code{fhpredict:::remove_and_reorder_columns()}
#' @keywords internal
#'
api_get_timeseries <- function(
  path, subject = "timeseries", sort = TRUE, token = NULL, type = NULL
)
{
  df <- kwb.utils::catAndRun(
    get_text("reading_data", subject = subject),
    expr = {
      result <- safe_postgres_get(path, token = token)
      flatten_recursive_list(result$data)
    }
  )

  if (is.null(df)) {
    return(data.frame())
  }

  # Converting time columns
  df <- convert_time_columns(df)
  df$dateTime <- get_date_time_from_text_columns(df)

  if (sort) {
    df <- df[order(df$dateTime), , drop = FALSE]
  }

  if (is.null(type)) {
    kwb.utils::removeColumns(df, c("date", "comment"))
  } else {
    remove_and_reorder_columns(df, type)
  }
}

# get_date_time_from_text_columns ----------------------------------------------
get_date_time_from_text_columns <- function(df)
{
  get <- kwb.utils::selectColumns

  date_strings <- get(df, "date")

  timestamps <- if (grepl("Z$", date_strings[1])) {

    date_strings

  } else {

    clean_stop(
      "timestamps do not end with 'Z' as expected in ",
      "get_date_time_from_text_columns()"
    )
    #paste(substr(date_strings, 1, 10), get(df, "dateTime"))
  }

  as.POSIXct(x = timestamps, tz = "Europe/Berlin")
}
