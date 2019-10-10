# add_timeseries_point_to_database ---------------------------------------------
add_timeseries_point_to_database <- function(
  path, date_string, time_string, value, comment = NULL, data = NULL
)
{
  body <- if (is.null(data)) {

    list(
      value = value,
      dateTime = time_string,
      date = date_string,
      comment = comment
    )

  } else {

    # Create a list of lists if more than one record is given
    lapply(seq_len(nrow(data)), function(i) as.list(data[i, ]))
  }

  result <- postgres_post(path, body)

  response <- attr(result, "response")

  if (! is.null(response) && response$status_code == 500) {

    error <- kwb.utils::selectElements(httr::content(response), "error")

    stop(paste(error$message, error$detail, sep = "\n"))
  }

  #stop_on_request_failure(result)

  # Return the id(s) of the added record(s)
  sapply(result$data, kwb.utils::selectElements, "id")
}
