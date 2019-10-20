# add_timeseries_to_database ---------------------------------------------------
add_timeseries_to_database <- function(path, data)
{
  stopifnot(is.data.frame(data))

  n_records <- nrow(data)

  if (n_records == 0) {
    message(get_text("nothing_to_add"))
    return(integer())
  }

  # Helper function to convert a row of a data frame to a list
  one_row_as_list <- function(i) as.list(data[i, , drop = FALSE])

  # POST the data.
  # Convert data frame to a list of lists if more than one record is given.
  result <- safe_postgres_post(path, body = if (n_records > 1) {
    lapply(seq_len(n_records), one_row_as_list)
  } else {
    one_row_as_list(1)
  })

  # Return the id(s) of the added record(s)
  sapply(result$data, kwb.utils::selectElements, "id")
}
