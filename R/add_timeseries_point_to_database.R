# add_timeseries_point_to_database ---------------------------------------------
add_timeseries_point_to_database <- function(
  path, date_string, time_string, value, comment = NULL, subject = "time series"
)
{
  body <- list(
    value = value,
    dateTime = time_string,
    date = date_string,
    comment = comment
  )

  result <- postgres_post(path, body)

  response <- attr(result, "response")

  if (! is.null(response) && response$status_code == 500) {

    error <- kwb.utils::selectElements(httr::content(response), "error")

    stop(paste(error$message, error$detail, sep = "\n"))
  }

  #stop_on_request_failure(result)

  # Return the id of the added record
  result$data[[1]]$id
}
