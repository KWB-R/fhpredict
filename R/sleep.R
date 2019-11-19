#' Wait a Given Number of Seconds
#'
#' @param seconds time to wait, in seconds
#' @export
#' @return list with elements \code{data}, \code{success}, \code{message}
sleep <- function(seconds)
{
  Sys.sleep(0.1)

  create_result(message = paste("Back after waiting for", seconds, "seconds"))
}
