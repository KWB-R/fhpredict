#' A io function
#'
#' simple function to test the opencpu API for the project
#' @param spot_id number
#' @param user_id number
#' @export
#' @keywords simple
#' @examples
#' \dontrun{simple()}
simple <- function (spot_id, user_id)
{
  Sys.sleep(0.5)

  result <- list(
    body = list(
      strArr = c(spot_id, user_id),
      numArr = c(1, 2, 3)
    ),
    str = 'baz',
    success = TRUE
  )

  jsonlite::toJSON(result, auto_unbox = TRUE)
}
