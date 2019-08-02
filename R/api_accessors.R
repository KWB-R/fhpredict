if (FALSE)
{
  str(river_data, 2)

  clear_cache()

  # What users are available?
  users <- api_users()

  # Get purification plant data
  spots <- get_bathingspots_for_user(user_id = 3)

  result <- fhpredict:::safe_postgres_get("users/3/bathingspots/2430/purificationPlants")

  result <- fhpredict:::safe_postgres_get("users/3/bathingspots/18/purificationPlants/1/measurement")

  # Measurements from bathing spots: ok
  measurements <- get_measurements_at_bathing_spot(user_id = 3, spot_id = 18)

  # Measurements from purification plats: Not yet implemented
  result <- fhpredict::postgres_post(
    path = "users/3/bathingspots/18/purificationPlants/1/measurement",
    body = list(
      date = "2019-01-31",
      dateTime = "10:00:00",
      value = 123,
      comment = "test data added by Hauke"
    )
  )

  # Generic measurements
  result <- fhpredict:::safe_postgres_get("users/3/bathingspots/18/genericInputs/1")

  result
}

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

    path <- sprintf("users/%d/bathingspots/%d/measurements", user_id, spot_id)

    result <- safe_postgres_get(path)

    measurements <- kwb.utils::safeRowBindAll(lapply(result$data, function(x) {
      kwb.utils::asNoFactorDataFrame(kwb.utils::excludeNULL(x, dbg = FALSE))
    }))

    kwb.utils::removeColumns(measurements, pattern = "At$")
  })
}

# api_bathingspots -------------------------------------------------------------
api_bathingspots <- function(user_id = -1, pattern = "")
{
  users <- api_users()

  if (user_id == -1 || ! user_id %in% users$id) {
    print(kwb.utils::selectColumns(users, c("id", "firstName", "lastName")))
    clean_stop("Invalid user_id: ", user_id, ". See above for possible users.")
  }

  spots <- run_cached(name = paste0("bathingspots", user_id), {

    result <- safe_postgres_get(sprintf("users/%d/bathingspots", user_id))

    spots <- do.call(rbind, lapply(result$data, function(x) {
      kwb.utils::asNoFactorDataFrame(
        kwb.utils::selectElements(x, c("id", "name"))
      )
    }))
  })

  if (pattern == "") {
    return(spots)
  }

  spots[grepl(pattern, spots$name, ignore.case = TRUE), ]
}
