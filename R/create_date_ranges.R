# create_date_ranges -----------------------------------------------------------
create_date_ranges <- function(dates, offset = -5)
{
  stopifnot(inherits(dates, "Date"))

  date_sequences <- lapply(dates, function(date) {
    seq(date + offset, date, by = 1)
  })

  unique_dates <- sort(unique(do.call(c, date_sequences)))

  ids_break <- which(diff(unique_dates) > 1)
  ids_to <- unique(c(ids_break, length(unique_dates)))
  ids_from <- unique(c(1, ids_break + 1))

  lapply(seq_along(ids_from), function(i) {
    c(unique_dates[ids_from[i]], unique_dates[ids_to[i]])
  })
}
