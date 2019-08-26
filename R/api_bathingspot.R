# api_get_bathingspot ----------------------------------------------------------

#' Get Bathing Spot(s)
#'
#' @param user_id user ID or -1L (all users)
#' @param spot_id bathing spot ID or -1L (all bathing spots)
#' @param limit passed to \code{fhpredict:::path_bathingspot}
#' @param skip passed to \code{fhpredict:::path_bathingspot}
#' @param pattern optional. Pattern matching the names of bathing spots to be
#'   returned
#' @export
api_get_bathingspot <- function(
  user_id = -1L, spot_id = -1L, limit = NULL, skip = NULL, pattern = ""
)
{
  #kwb.utils::assignPackageObjects("fhpredict")
  #stopifnot(is.character(as))
  #stopifnot(length(as) == 1)
  #stopifnot(as %in% c("data.frame", "response"))

  check_user_id(user_id)

  #user_id = -1L; spot_id = -1L; limit = NULL; skip = NULL; pattern = ""
  #limit <- 2
  content <- safe_postgres_get(path_bathingspot(user_id, spot_id, limit, skip))

  # if (as == "response") {
  #   return(content)
  # }

  data <- kwb.utils::selectElements(content, "data")

  # If there are data for more than one bathing spot, return an overview
  if (length(data) > 1) {

    return(dplyr::bind_rows(lapply(data, extract_flat_information)))
  }

  spot_names <- unlist(lapply(data, kwb.utils::selectElements, "name"))

  # Apply the pattern if a pattern is given
  if (nzchar(pattern) && length(data)) {

    data <- data[grep(pattern, spot_names, ignore.case = TRUE)]
  }

  spot_names <- kwb.utils::hsSubstSpecChars(spot_names)

  names(data) <- kwb.utils::makeUnique(spot_names)

  lapply(data, function(x) {

    x <- kwb.utils::excludeNULL(x, dbg = FALSE)
    x$area_coordinates <- get_area_coordinates(x)
    x
  })
}
