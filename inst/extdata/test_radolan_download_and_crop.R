if (FALSE)
{
  # Function to be triggered by the user: Provide rain data for the bathing spot
  system.time(rain_ids <- fhpredict::provide_rain_data_for_bathing_spot(
    user_id = 3, spot_id = 1441, sampling_time = "1050",
    date_range = as.Date(c("2008-07-01", "2009-07-01"))
  ))

  # Read rain from database
  rain <- fhpredict::api_get_rain(user_id, spot_id)

  View(rain)

  # What should we do with duplicates?
  sum(duplicated(kwb.utils::pasteColumns(rain, c("date", "dateTime"))))

  # Delete rain data with given rain record ids
  fhpredict::api_delete_rain(user_id, spot_id, rain_ids)

  # For simplicity reasons: delete all rain data before inserting new data
  fhpredict::api_delete_rain(user_id, spot_id)

  # TODO: Remove only the duplicates, keeping the most current values
}
