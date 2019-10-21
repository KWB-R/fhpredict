# api_get_predictions ----------------------------------------------------------
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

# # api_replace_predictions ------------------------------------------------------
# api_replace_predictions <- function(user_id, spot_id, percentiles)
# {
#   get <- kwb.utils::selectColumns
#
#   # Set path to API endpoint
#   path <- path_predictions(user_id, spot_id)
#
#   # Read existing predictions from database
#   predictions_db <- api_get_timeseries(path)
#
#   # Find IDs that relate to days for which new data are available
#   if (nrow(predictions_db)) {
#
#     # Get IDs of records that exist for the days to be inserted
#     date_strings <- format(get(predictions_db, "dateTime"), "%Y-%m-%d")
#
#     is_replaced <- date_strings %in% as.character(get(percentiles, "dateTime"))
#
#     ids <- get(predictions_db, "id")[is_replaced]
#
#     # Clear existing predictions from the database
#     if (length(ids)) {
#       api_delete_timeseries(user_id, spot_id, path_predictions, ids = ids)
#     }
#   }
#
#   # Delete all predictions
#   api_delete_predictions(user_id, spot_id)
#
#   # Add predictions to the database
#   add_timeseries_to_database(path, data = percentiles)
# }

# get_percentiles_from_prediction ----------------------------------------------
get_percentiles_from_prediction <- function(prediction)
{
  probs <- c("P2.5" = 0.025, P50 = 0.5, P90 = 0.9, P95 = 0.95, "P97.5" = 0.975)

  percentiles <- 10^(t(apply(prediction, 2, stats::quantile, probs = probs)))

  stats::setNames(as.data.frame(percentiles), names(probs))
}

# get_quality_from_percentiles -------------------------------------------------
get_quality_from_percentiles <- function(percentiles)
{
  p90 <- kwb.utils::selectColumns(percentiles, "P90")
  p95 <- kwb.utils::selectColumns(percentiles, "P95")

  ifelse(
    p90 > 900, "mangelhaft",
    ifelse(
      p90 < 900 & p95 >= 1000, "ausreichend",
      ifelse(
        p95 < 1000 & p95 >= 500, "gut",
        "ausgezeichnet"
      )
    )
  )
}
