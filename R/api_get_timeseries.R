# api_get_timeseries -----------------------------------------------------------

#' Get Timeseries Data from the Postgres Database Via API
#'
#' @param path (relative) path to API endpoint
#' @param subject name of data subject, to be used in messages
#' @param sort if \code{TRUE} (the default), the returned data frame will be
#'   sorted by the "dateTime" column
#' @keywords internal
#'
api_get_timeseries <- function(path, subject = "timeseries", sort = TRUE)
{
  df <- kwb.utils::catAndRun(
    get_text("reading_data", subject = subject),
    expr = {
      result <- safe_postgres_get(path)
      flatten_recursive_list(result$data)
    }
  )

  if (is.null(df)) {

    return(data.frame())
  }

  df <- kwb.utils::catAndRun(
    get_text("converting_time"),
    expr = {
      df <- convert_time_columns(df)
      df$dateTime = get_date_time_from_text_columns(df)
      df
    }
  )

  if (sort) {

    df <- kwb.utils::catAndRun(
      get_text("sorting_by_time"),
      expr = df[order(df$dateTime), , drop = FALSE]
    )
  }

  kwb.utils::removeColumns(df, c("date", "comment"))
}

# get_date_time_from_text_columns ----------------------------------------------
get_date_time_from_text_columns <- function(df)
{
  get <- kwb.utils::selectColumns

  date_strings <- get(df, "date")

  timestamps <- if (grepl("Z$", date_strings[1])) {

    date_strings

  } else {

    paste(substr(date_strings, 1, 10), get(df, "dateTime"))
  }

  as.POSIXct(x = timestamps, tz = "Europe/Berlin")
}
