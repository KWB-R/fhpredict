# add_timeseries_point_to_database ---------------------------------------------
add_timeseries_point_to_database <- function(
  path, date_string, time_string, value, comment = NULL, subject = "time series"
)
{
  result <- postgres_post(
    path = path,
    body = list(
      value = value,
      dateTime = time_string,
      date = date_string,
      comment = comment
    )
  )

  stop_on_request_failure(result)

  id <- result$data[[1]]$id

  #message("A ", subject, " data record with id = ", id, " has been inserted.")

  # Return the id of the added record
  id
}
