#' Download Radolan Files on DWD Server
#'
#' @param resolution temporal resolution, one of "daily" or "hourly" (default:
#'   "daily")
#' @param export_dir export directory (default: "data" in current working
#' directory)
#' @importFrom magrittr %>%
#' @importFrom kwb.utils catAndRun
#' @importFrom fs dir_create
#' @importFrom utils download.file
#' @return list with "daily_historical_urls" and "hourly_historical_urls"
#' @export
#' @examples
#' \dontrun{download_radolan(resolution = "daily")}
download_radolan <- function(resolution = "daily", export_dir = "data") {

  if (! resolution %in% c("daily", "hourly")) {
    clean_stop("resolution must be one of 'daily', 'hourly'")
  }

  # Define helper function
  download_historical <- function(url, resolution) {

    hist_dir <- sprintf("%s/%s/historical", resolution, export_dir)

    fs::dir_create(hist_dir, recursive = TRUE)

    export_path <- sprintf("%s/%s", hist_dir, basename(url))

    kwb.utils::catAndRun(
      messageText = sprintf(
        'Download: "%s, historical" and save to %s', resolution, export_path
      ),
      newLine = 3,
      expr = try(
        utils::download.file(url = url, destfile = export_path, mode = "wb")
      )
    )
  }

  kwb.utils::catAndRun(
    messageText = sprintf("Download: '%s' historical radolan data", resolution),
    newLine = 3,
    expr = {
      sapply(
        X = get_radolan_urls()[[paste0(resolution, "_historical_urls")]],
        FUN = download_historical,
        resolution = resolution
      )
    }
  )
}
