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
#' @param check if \code{TRUE} (the default) it is checked first whether the
#'   given \code{user_id} exists. Switching this check off may speed up the
#'   request.
#' @export
api_get_bathingspot <- function(
  user_id = -1L, spot_id = -1L, limit = NULL, skip = NULL, pattern = "",
  check = TRUE
)
{
  #kwb.utils::assignPackageObjects("fhpredict")
  #user_id = -1L; spot_id = -1L; limit = NULL; skip = NULL; pattern = ""
  #limit <- 2

  if (check) {
    check_user_id(user_id)
  }

  content <- safe_postgres_get(path_bathingspot(user_id, spot_id, limit, skip))

  data <- kwb.utils::selectElements(content, "data")

  # If there are data for more than one bathing spot, return an overview
  if (length(data) > 1L) {

    # Apply the pattern if a pattern is given
    if (nzchar(pattern)) {

      spot_names <- unlist(lapply(data, kwb.utils::selectElements, "name"))

      data <- data[grep(pattern, spot_names, ignore.case = TRUE)]
    }

    bathingspots <- dplyr::bind_rows(lapply(data, extract_flat_information))

    return(remove_and_reorder_columns(bathingspots, "bathingspots"))
  }

  # If we arrive here, there is only one list element
  if (length(data) != 1L) clean_stop(
    "The bathing spot object does not have a length of one as expected\n",
    sprintf("(user_id = %d, spot_id = %d). ", user_id, spot_id),
    "The R structure of the object is:\n",
    structure_as_text(data)
  )

  result <- data[[1L]]

  result$area_coordinates <- get_area_coordinates(result)

  result
}
