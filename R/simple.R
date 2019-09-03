#' A io function
#'
#' simple function to test the opencpu API for the project
#' @param a string (optional)
#' @param b string (optional)
#' @export
#' @keywords simple
#' @examples
#' \dontrun{simple()}
simple <- function (a = 'foo', b = 'bah') {
  Sys.sleep(0.5)
  nums = c(1, 2, 3)
  res <- list(
    body = list(
      strArr = c(a, b),
      numArr = nums
    ),
    str = 'baz',
    success = TRUE
  )
  jsonlite::toJSON(res, auto_unbox = TRUE)
}
