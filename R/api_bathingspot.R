# api_get_bathingspot ----------------------------------------------------------

#' Get Bathing Spot(s)
#'
#' @param user_id user ID or -1L (all users)
#' @param spot_id bathing spot ID or -1L (all bathing spots)
#' @param limit passed to \code{fhpredict:::path_bathingspot}
#' @param skip passed to \code{fhpredict:::path_bathingspot}
#' @param pattern optional. Pattern matching the names of bathing spots to be
#'   returned. Only considered if no \code{spot_id} is given (i.e. has its
#'   default value \code{-1L}).
#' @export
api_get_bathingspot <- function(
  user_id = -1L, spot_id = -1L, limit = NULL, skip = NULL, pattern = ""
)
{
  #kwb.utils::assignPackageObjects("fhpredict")
  #user_id = -1L; spot_id = -1L; limit = NULL; skip = NULL; pattern = ""
  #limit <- 2

  check_user_id(user_id)

  path <- path_bathingspot(user_id, spot_id, limit, skip)

  content <- safe_postgres_get(path)

  data <- kwb.utils::selectElements(content, "data")

  # If there are data for more than one bathing spot, return an overview
  if (length(data) > 1) {

    # Apply the pattern if a pattern is given
    if (nzchar(pattern)) {

      spot_names <- unlist(lapply(data, kwb.utils::selectElements, "name"))

      data <- data[grep(pattern, spot_names, ignore.case = TRUE)]
    }

    return(dplyr::bind_rows(lapply(data, extract_flat_information)))
  }

  # If we arrive here, there is only one list element
  stopifnot(length(data) == 1)

  result <- data[[1]]

  result$area_coordinates <- get_area_coordinates(result)

  result
}
