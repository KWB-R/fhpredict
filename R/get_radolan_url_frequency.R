# get_radolan_url_frequency ----------------------------------------------------
get_radolan_url_frequency <- function(
  years = 2008:2019, bathing_season_only = TRUE
)
{
  # Get URLs to all available downloaded Radolan files
  urls_list <- lapply(stats::setNames(nm = years), function(year) {
    message("Getting URLs to Radolan files for ", year)
    urls <- get_radolan_urls_bucket(
      year, bathing_season_only = bathing_season_only
    )
  })

  days_all <- format(
    x = seq(as.Date("2000-01-01"), as.Date("2000-12-31"), by = 1),
    format = "%m%d"
  )

  if (bathing_season_only) {
    days_all <- days_all[days_all >= "0501" & days_all < "1001"]
  }

  backbone <- kwb.utils::noFactorDataFrame(day = days_all)

  stats <- lapply(stats::setNames(nm = names(urls_list)), function(year_label) {

    day <- substr(names(urls_list[[year_label]]), 5, 8)

    if (length(day) == 0) {

      return(cbind(backbone, Freq = NA_integer_))
    }

    dplyr::left_join(
      backbone,
      kwb.utils::asNoFactorDataFrame(table(day)),
      by = "day"
    )
  })

  n_per_day <- kwb.utils::mergeAll(stats, by = "day", dbg = FALSE)

  n_per_day[is.na(n_per_day)] <- 0

  stats::setNames(n_per_day, gsub("Freq\\.", "", names(n_per_day)))
}
