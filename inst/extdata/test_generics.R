#
# Create generic input, add generic measurements, delete generic measurements,
# delete generic input
#
# author: Hauke Sonnenberg
# created-on: 2019-11-29
#

user_id <- 9
spot_id <- 1

fhpredict:::api_get_generic(user_id, spot_id)

generic_id <- fhpredict:::api_add_generic(user_id, spot_id, "generic_1")

data <- data.frame(dateTime = "14:15:16", date = "2019-11-30", value = 234)

fhpredict:::api_add_generic_measurements(user_id, spot_id, generic_id, data)

fhpredict:::api_get_generic_measurements(user_id, spot_id, generic_id)

fhpredict:::api_delete_generic_measurements(user_id, spot_id, generic_id)

fhpredict:::api_delete_generic(user_id, spot_id, generic_id)
