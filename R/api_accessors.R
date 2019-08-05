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

# api_get_bathingspot ----------------------------------------------------------

#' Get Bathing Spot(s)
#'
#' @param user_id user ID or -1L (all users)
#' @param spot_id bathing spot ID or -1L (all bathing spots)
#' @param pattern optional. Pattern matching the names of bathing spots to be
#'   returned
#' @export
api_get_bathingspot <- function(user_id = -1L, spot_id = -1L, pattern = "")
{
  #kwb.utils::assignPackageObjects("fhpredict")
  users <- api_users()

  if (user_id != -1L && ! user_id %in% users$id) {
    print(kwb.utils::selectColumns(users, c("id", "firstName", "lastName")))
    clean_stop("Invalid user_id: ", user_id, ". See above for possible users.")
  }

  spots <- run_cached(
    name = path_bathingspot(user_id, spot_id, sep = "-"),
    expr = {
      result_all <- postgres_get(path_bathingspot(user_id, spot_id))
      result_user <- postgres_get(path_bathingspot(user_id = 3, spot_id))
      result_spot <- postgres_get(path_bathingspot(spot_id = 111))
      result_user_spot <- postgres_get(path_bathingspot(user_id = 3, spot_id = 111))

      str(result_all, 1)
      str(result_user, 1)
      str(result_spot, 1)
      str(result_user_spot, 1)

      spots <- do.call(rbind, lapply(result$data, function(x) {
        kwb.utils::asNoFactorDataFrame(
          kwb.utils::selectElements(x, c("id", "name"))
        )
      }))
    }
  )

  if (pattern == "") {
    return(spots)
  }

  spots[grepl(pattern, spots$name, ignore.case = TRUE), ]
}
