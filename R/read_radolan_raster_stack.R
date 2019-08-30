# read_radolan_raster_stack ----------------------------------------------------
read_radolan_raster_stack <- function(
  date_from, date_to, bathing_season_only, sampling_time
)
{
  # Get URLs to Radolan files
  urls <- get_radolan_urls_bucket(
    from = as.character(date_from),
    to = as.character(date_to),
    time = sampling_time,
    bathing_season_only = bathing_season_only
  )

  # Download, read and stack Radolan files
  raster::stack(lapply(urls, function(url) {
    message("Reading ", url)
    kwb.dwd::read_binary_radolan_file(url)
  }))
}
