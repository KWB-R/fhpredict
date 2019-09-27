# get_radolan_urls_in_date_ranges ----------------------------------------------
get_radolan_urls_in_date_ranges <- function(date_ranges, time = "1050")
{
  dates_from <- kwb.utils::selectColumns(date_ranges, "from_date")
  dates_to <- kwb.utils::selectColumns(date_ranges, "to_date")

  yyyymmdd <- function(date) gsub("-", "", as.character(date))

  lapply(seq_along(dates_from), function(i) {

    kwb.utils::catAndRun(
      sprintf(
        "Getting URLs to files between %s and %s", dates_from[i], dates_to[i]
      ),
      fhpredict::get_radolan_urls_bucket(
        from = yyyymmdd(dates_from[i]),
        to = yyyymmdd(dates_to[i]),
        time = time
      )
    )
  })
}
