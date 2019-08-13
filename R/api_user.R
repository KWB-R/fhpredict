# api_get_users ----------------------------------------------------------------
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
