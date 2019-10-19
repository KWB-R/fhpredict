# get_radolan_urls_in_date_ranges ----------------------------------------------
# urls <- get_radolan_urls_in_date_ranges(
#   data.frame(from_date = "2018-07-01", to_date = "2018-08-01")
# )
get_radolan_urls_in_date_ranges <- function(date_ranges, time = "1050")
{
  dates_from <- kwb.utils::selectColumns(date_ranges, "from_date")
  dates_to <- kwb.utils::selectColumns(date_ranges, "to_date")

  yyyymmdd <- function(date) gsub("-", "", as.character(date))

  lapply(seq_along(dates_from), function(i) {

    kwb.utils::catAndRun(
      get_text(
        "getting_radolan_urls_between", from = dates_from[i], to = dates_to[i]
      ),
      get_radolan_urls_bucket(
        from = yyyymmdd(dates_from[i]),
        to = yyyymmdd(dates_to[i]),
        time = time
      )
    )
  })
}
