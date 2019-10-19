# get_radolan_url_frequency ----------------------------------------------------
get_radolan_url_frequency <- function(
  years = 2008:2019, bathing_season_only = TRUE
)
{
  # Get URLs to all available downloaded Radolan files
  urls_list <- get_radolan_urls_for_years(
    years = years, time = "", bathing_season_only = bathing_season_only,
    days_before_start = 0L
  )

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

# get_radolan_urls_for_years ---------------------------------------------------
# days_before_start: number of days to consider before start of bathing season
get_radolan_urls_for_years <- function(
  years, time = "1050", bathing_season_only = TRUE, days_before_start = 5L
)
{
  #time = "1050";bathing_season_only = TRUE;days_before_start = 5L
  yyyymmdd <- function(x) format(x, "%Y%m%d")

  first_day <- ifelse(bathing_season_only, "-05-01", "-01-01")
  last_day <- ifelse(bathing_season_only, "0930", "1231")

  season_info <- ifelse(bathing_season_only, " (bathing season only)", "")

  lapply(stats::setNames(nm = sort(unique(years))), function(year) {

    kwb.utils::catAndRun(
      get_text("getting_radolan_urls", year = year, season_info = season_info),
      get_radolan_urls_bucket(
        from = yyyymmdd(as.Date(paste0(year, first_day)) - days_before_start),
        to = paste0(year, last_day),
        time = time,
        bathing_season_only = FALSE
      )
    )}
  )
}
