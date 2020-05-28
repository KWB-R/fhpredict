# api_get_predictions ----------------------------------------------------------

#' Get Predictions
#'
#' @param user_id user ID or -1L (all users)
#' @param spot_id bathing spot ID or -1L (all bathing spots)
#' @export
api_get_predictions <- function(user_id, spot_id)
{
  path <- path_predictions(user_id, spot_id)
  api_get_timeseries(path, type = "predictions")
}

#' Delete Prediction Data from Postgres Database Via API
#'
#' @param user_id user id
#' @param spot_id bathing spot id
#' @param ids optional. Vector of prediction ids. If not given or \code{NULL}
#'   (the default) all predictions for the bathing spot are deleted!
#' @param dbg if \code{TRUE} debug messages are shown
#' @export
#'
api_delete_predictions <- function(user_id, spot_id, ids = NULL, dbg = TRUE)
{
  api_delete_timeseries(
    user_id = user_id,
    spot_id = spot_id,
    ids = ids,
    path_function = path_predictions,
    subject = "predictions"
  )
}

# api_replace_predictions ------------------------------------------------------
api_replace_predictions <- function(user_id, spot_id, percentiles)
{
  #kwb.utils::assignPackageObjects("fhpredict")
  #user_id=8;spot_id=43
  #percentiles <- data.frame(dateTime = as.POSIXct(c("2020-03-11", "2020-03-12")))
  #predictions_db <- data.frame(id = 123L, dateTime = as.POSIXct(c("2020-03-11")))

  get <- kwb.utils::selectColumns

  # Path to API endpoint
  path <- path_predictions(user_id, spot_id)

  # Read existing predictions from the database
  predictions_db <- api_get_timeseries(path)

  # Date strings of the predictions in the database
  date_strings_db <- if (nrow(predictions_db)) {
    format(get(predictions_db, "dateTime"), "%Y-%m-%d")
  } else {
    character()
  }

  # Date strings of the new predictions
  date_strings_new <- as.character(get(percentiles, "dateTime"))

  # In what rows of predictions_db do we find the dates of the new predictions?
  prediction_rows <- match(date_strings_new, date_strings_db)

  # Which of the new predictions are already in the database?
  in_db <- ! is.na(prediction_rows)

  # Replace the existing records if there are any
  ids_updated <- if (any(in_db)) {

    unlist(lapply(which(in_db), function(i) {
      #i <- 1L

      # ID of corresponding record in the database
      prediction_id <- get(predictions_db, "id")[prediction_rows[i]]

      # Update the record in the database with a PUT request
      postgres_put(
        path = path_predictions(user_id, spot_id, prediction_id),
        body = as.list(percentiles[i, ])
      )

      prediction_id
    }))

  } else {

    integer()
  }

  # Insert new records if there are any
  ids_added <- if (any(! in_db)) {

    # Add predictions to the database
    add_timeseries_to_database(path, data = percentiles[! in_db, ])

  } else {

    integer()
  }

  # Return the ids of the updated or added prediction IDs. Set attributes
  # "updated" and "added", containing the subsets of IDs that have been updated
  # and added, respectively
  structure(
    c(ids_updated, ids_added),
    updated = ids_updated,
    added = ids_added
  )
}

# get_percentiles_from_prediction ----------------------------------------------
get_percentiles_from_prediction <- function(prediction)
{
  probs <- c("P2.5" = 0.025, P50 = 0.5, P90 = 0.9, P95 = 0.95, "P97.5" = 0.975)

  percentiles <- 10^(t(apply(prediction, 2, stats::quantile, probs = probs)))

  stats::setNames(as.data.frame(percentiles), names(probs))
}

# get_quality_from_percentiles -------------------------------------------------
get_quality_from_percentiles <- function(percentiles, version = 1)
{
  p90 <- kwb.utils::selectColumns(percentiles, "P90")
  p95 <- kwb.utils::selectColumns(percentiles, "P95")

  if (version == 1) return(
    ifelse(
      p90 > 900,
      "mangelhaft",
      ifelse(
        p90 < 900 & p95 >= 1000,
        "ausreichend",
        ifelse(
          p95 < 1000 & p95 >= 500,
          "gut",
          "ausgezeichnet"
        )
      )
    )
  )

  if (version == 2) return(

    ifelse(
      p95 < log10(500),
      "ausgezeichnet",
      ifelse(
        p95 < log10(1000) & p90 < log10(900),
        "gut",
        ifelse(
          p95 > log10(1000) & p90 < log10(900),
          "ausreichend",
          "mangelhaft"
        )
      )
    )
  )

  clean_stop("version must be either 1 or 2 but was: ", version)
}
