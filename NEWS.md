# fhpredict 0.9.0 (2019-11-19)

* add plot functions
* add sleep()
* use R package aws.s3 to download Radolan files

# fhpredict 0.7.0

* consider global irradiances
* post actual prediction (5 last days of bathing season) instead of fake 
  prediction
* add get_data_summary()
* add api_delete_predictions()
* delete all rain data before providing new rain data
* remove invalid e.coli values -1 values befor
* allow to switch between raster::crop() and mask() 

# fhpredict 0.5.0

* provide_rain_data_for_bathing_spot(): Return object with elements "success",
  "data", "message"
* fix bug in overwriting existing rain data

# fhpredict 0.4.0

* New: build_model(user_id, spot_id)
* New: predict_quality(user_id, spot_id) - with fake implementation!

# fhpredict 0.3.0

# fhpredict 0.2.0

# fhpredict 0.1.0

# fhpredict 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.

* see https://style.tidyverse.org/news.html for writing a good `NEWS.md`
