# check_bathingspot ------------------------------------------------------------
check_bathingspot <- function(bathing_spot)
{
  stopifnot(is.list(bathing_spot))

  # Use selectElements() to check if expected elements exist
  kwb.utils::selectElements(bathing_spot, elements = c(
    "id", "name", "latitude", "longitude", "area"
  ))
}

# check_user_id ----------------------------------------------------------------
check_user_id <- function(user_id)
{
  if (user_id == -1L) {
    return()
  }

  users <- api_get_users()

  if (! user_id %in% users$id) {

    print(kwb.utils::selectColumns(users, c("id", "firstName", "lastName")))

    clean_stop(get_text("invalid_user", user_id = user_id))
  }
}
