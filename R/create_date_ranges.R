# group_dates_by_diff ----------------------------------------------------------

#' @examples
#' group_dates_by_diff(dates, diff_days = 5)
group_dates_by_diff <- function(dates, diff_days = 1)
{
  stopifnot(inherits(dates, "Date"))
  stopifnot(! is.unsorted(dates))

  ids_break <- which(diff(dates) > diff_days)
  ids_to <- unique(c(ids_break, length(dates)))
  ids_from <- unique(c(1, ids_break + 1))

  data.frame(
    from_id = ids_from,
    to_id = ids_to,
    from_date = dates[ids_from],
    to_date = dates[ids_to],
    n_days = as.integer(dates[ids_to]) - as.integer(dates[ids_from]) + 1
  )
}

# add_days_before --------------------------------------------------------------

#' @examples
#' dates <- as.Date(c("2019-07-05", "2019-07-10"))
#' add_days_before(dates)
#' add_days_before(dates, n_days_before = 2)
#' add_days_before(dates, n_days_before = 10) # duplicates are avoided
add_days_before <- function(dates, n_days_before = 1)
{
  stopifnot(inherits(dates, "Date"))

  date_sequences <- lapply(dates, function(date) {
    seq(date - n_days_before, date, by = 1)
  })

  sort(unique(do.call(c, date_sequences)))
}
