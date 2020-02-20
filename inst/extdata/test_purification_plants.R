#
# Create purification plant, add measurements to purification platn, delete
# measurements of purification plants, delete purification plant
#
# author: Hauke Sonnenberg
# created-on: 2019-11-29
#

user_id <- 9
spot_id <- 1

fhpredict:::api_get_plant(user_id, spot_id)

plant_id <- fhpredict:::api_add_plant(user_id, spot_id, "my plant 2")

data <- data.frame(dateTime = "14:15:16", date = "2019-11-30", value = 234)

fhpredict:::api_add_plant_measurements(user_id, spot_id, plant_id, data)

fhpredict:::api_get_plant_measurements(user_id, spot_id, plant_id)

fhpredict:::api_delete_plant_measurements(user_id, spot_id, plant_id)

fhpredict:::api_delete_plant(user_id, spot_id, plant_id)
