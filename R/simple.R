#' A io function
#'
#' simple function to test the opencpu API for the project
#' @param spot_id number
#' @param user_id number
#' @export
#' @keywords simple
#' @examples
#' \dontrun{simple()}
simple <- function (spot_id, user_id) {
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
