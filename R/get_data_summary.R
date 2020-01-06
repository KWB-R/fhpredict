# get_data_summary -------------------------------------------------------------

#' Get Overview on Available Data
#'
#' @param user_id user identifier (see \code{\link{api_get_users}} for available
#'   users)
#' @param spot_id bathing spot identifier (see
#'   \code{\link{api_get_bathingspot}} for available bathing spots
#' @return data frame with one row per type of data and columns \code{from}
#'   (date of earliest record), \code{to} (date of latest record), \code{n}
#'   (number of records)
#' @export
get_data_summary <- function(user_id, spot_id)
{
  rain <- api_get_rain(user_id, spot_id)
  hygiene <- api_get_measurements(user_id, spot_id)
  discharges <- api_get_discharge(user_id, spot_id)
  irradiances <- api_get_irradiances(user_id, spot_id)
  predictions <- api_get_predictions(user_id, spot_id)

  get_date_range <- function(x) {

    date_range <- if (is.null(x)) {
      as.Date(c(NA, NA))
    } else {
      range(as.Date(substr(x, 1, 10)))
    }

    data.frame(from = date_range[1], to = date_range[2], n = length(x))
  }

  (ranges <- list(
    hygiene = get_date_range(hygiene$date),
    rain = get_date_range(rain$dateTime),
    discharges = get_date_range(discharges$dateTime),
    irradiances = get_date_range(irradiances$dateTime),
    predictions = get_date_range(predictions$dateTime)
  ))

  # Look for purification plant measurements and generic inputs
  prefixes <- c(plant = "purification_plant", generic = "gneric_input")

  for (type in names(prefixes)) {

    #type <- "plant"
    measurements <- collect_series_measurements(
      type = type, prefix = prefixes[type], user_id, spot_id
    )

    if (length(measurements)) {

      ranges <- c(ranges, lapply(measurements, function(df) {
        get_date_range(df$dateTime)
      }))
    }
  }

  do.call(rbind, ranges)
}
