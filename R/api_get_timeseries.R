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

    paste("Reading", subject, "data from database"), {

      result <- postgres_get(path)

      stop_on_request_failure(result)

      flatten_recursive_list(result$data)
    }
  )

  if (is.null(df)) {

    return(data.frame())
  }

  df <- kwb.utils::catAndRun(

    "Converting time columns from text to POSIXct", {

      df <- convert_time_columns(df)

      df$dateTime = as.POSIXct(
        x = kwb.utils::pasteColumns(df, c("date", "dateTime")),
        tz = "Europe/Berlin"
      )

      df
    }
  )

  if (sort) {

    df <- kwb.utils::catAndRun("Sorting data frame by time", {
      df[order(df$dateTime), , drop = FALSE]
    })
  }

  kwb.utils::removeColumns(df, c("date", "comment"))
}
