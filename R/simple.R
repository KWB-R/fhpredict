#' A io function
#'
#' simple function to test the opencpu API for the project
#' @param str A JSON string (optional)
#' @export
#' @keywords simple
#' @examples
#' \dontrun{simple()}
simple <- function (str = '{"foo":"bah"}') {
  Sys.sleep(0.5)

  jsonlite::toJSON(jsonlite::fromJSON(str), auto_unbox = TRUE)
}
