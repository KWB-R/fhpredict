# api_get_users ----------------------------------------------------------------

#' Get Overview on Available Users
#'
#' @return data frame with one row per user and variables describing the user,
#'   such as: \code{id}, \code{firstName}, \code{lastName}, \code{rold},
#'   \code{email}
#' @export
#'
api_get_users <- function()
{
  #kwb.utils::assignPackageObjects("fhpredict")
  result <- safe_postgres_get("users")

  users <- dplyr::bind_rows(lapply(
    kwb.utils::selectElements(result, "data"), extract_flat_information
  ))

  first_columns <- c("id", "firstName", "lastName", "role")

  kwb.utils::moveColumnsToFront(users, first_columns)
}
