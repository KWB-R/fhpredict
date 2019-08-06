# api_users --------------------------------------------------------------------
api_users <- function()
{
  run_cached(name = "users", {

    result <- safe_postgres_get("users")

    users <- kwb.utils::safeRowBindAll(lapply(
      result$data, flatten_recursive_list
    ))

    first_columns <- c("id", "firstName", "lastName", "role")

    kwb.utils::moveColumnsToFront(users, first_columns)
  })
}

# api_measurements_spot --------------------------------------------------------
api_measurements_spot <- function(user_id = -1, spot_id = -1)
{
  run_cached(name = "measurements_spot", {

    path <- path_measurements(user_id, spot_id)

    result <- safe_postgres_get(path)

    measurements <- kwb.utils::safeRowBindAll(lapply(result$data, function(x) {
      kwb.utils::asNoFactorDataFrame(kwb.utils::excludeNULL(x, dbg = FALSE))
    }))

    kwb.utils::removeColumns(measurements, pattern = "At$")
  })
}

# check_user_id ----------------------------------------------------------------
check_user_id <- function(user_id)
{
  if (user_id == -1L) {
    return()
  }

  users <- api_users()

  if (! user_id %in% users$id) {

    print(kwb.utils::selectColumns(users, c("id", "firstName", "lastName")))

    clean_stop(sprintf(
      "Invalid user_id: %d. See above for possible users.", user_id
    ))
  }
}
