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

  # Get the list of users
  user_list <- kwb.utils::selectElements(result, "data")

  # Convert the list of users to a data frame
  users <- dplyr::bind_rows(lapply(user_list, extract_flat_information))

  # Remove columns of no interest and reorder columns
  remove_and_reorder_columns(users, "users")
}
