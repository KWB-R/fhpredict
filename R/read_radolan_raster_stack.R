# read_radolan_raster_stack ----------------------------------------------------
read_radolan_raster_stack <- function(urls)
{
  # Download, read and stack Radolan files
  raster::stack(lapply(urls, function(url) {
    message("Reading ", url)
    kwb.dwd::read_binary_radolan_file(url)
  }))
}
