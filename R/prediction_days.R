# set_prediction_days ----------------------------------------------------------
set_prediction_days <- function(days = NULL, clear = FALSE, user_id = 3L)
{
  #kwb.utils::assignPackageObjects("fhpredict")
  spots <- api_get_bathingspot(user_id)
  spot_id <- spots$id[spots$name == "Vorhersagedatum"]

  if (clear) {
    api_delete_measurements(user_id, spot_id)
    return()
  }

  if (is.null(days)) {
    measurements <- api_get_measurements(user_id, spot_id)
    return(measurements$date)
  }

  api_delete_measurements(user_id, spot_id)

  add_timeseries_to_database(
    path = path_measurements(user_id, spot_id),
    data = data.frame(date = days)
  )
}
