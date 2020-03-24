# Test area --------------------------------------------------------------------
if (FALSE)
{
  #kwb.utils::assignPackageObjects("fhpredict")

  model_overview <- fhpredict:::get_model_overview(9)
}

# get_model_overview -----------------------------------------------------------
get_model_overview <- function(user_id = -1L)
{
  # There must be exactly one user ID of numeric type
  stopifnot(is.numeric(user_id), length(user_id) == 1L)

  # Make sure that the user ID is an integer
  user_id <- as.integer(user_id)

  # If no user is given, call this function for all available users
  if (user_id == -1L) {

    user_ids <- kwb.utils::selectColumns(api_get_users(), "id")

    return(dplyr::bind_rows(lapply(user_ids, get_model_overview)))
  }

  #user_id <- 9L

  cat(sprintf("user_id: %d\n", user_id))

  check_user_id(user_id)

  spots <- try(api_get_bathingspot(user_id, check = FALSE), silent = TRUE)

  if (inherits(spots, "try-error")) {

    message("There seem to be no bathing spots for user_id = ", user_id)

  } else {

    spot_ids <- kwb.utils::selectColumns(spots, "id")

    spot_models <- lapply(spot_ids, api_get_model, user_id = user_id)

    merge_spot_models(spot_models, spot_ids, user_id)
  }
}

# merge_spot_models ------------------------------------------------------------
merge_spot_models <- function(spot_models, spot_ids, user_id)
{
  stopifnot(is.list(spot_models), is.integer(spot_ids), is.integer(user_id))
  stopifnot(length(spot_models) == length(spot_ids))

  has_model <- ! sapply(spot_models, is.null)

  dplyr::bind_rows(lapply(which(has_model), function(i) {

    #i <- 40L

    model_data_raw <- kwb.utils::renameColumns(
      spot_models[[i]], list(id = "model_id")
    )

    model_data <- unique(
      kwb.utils::removeColumns(model_data_raw, pattern = "^plotfiles")
    )

    cbind(user_id = user_id, spot_id = spot_ids[i], model_data)
  }))
}
