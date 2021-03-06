# fhpredict latest developments

# fhpredict 0.16.0 (2020-10-26)

* rename determine_days_to_predict() to determine_date_range()
* fix bug in predict_quality(): Distinguish between the range of days to be
  predicted and the range of days of which to load data (one day less)
* changes by Wolfgang Seis: fix bug in test_beta(), change model 
  selection/sorting

# fhpredict 0.15.0 (2020-05-28)

* predict_quality():
    + Set the defaults for both "from" and "to" to the current day. 
    + Add argument "radolan_time" that is passed to import_new_data(). By 
    default "radolan_time" is set to the last available time for the day given 
    in "from".

# fhpredict 0.14.0 (2020-05-28)

* Fix bug in api_replace_predictions(): do not fail on missing data in db
* Fix bug in get_independent_variables(): use all.vars()
* predict_quality(): Add argument "return_debug_info"
* provide_input_data():
    + Add argument "require_hygiene"
    + Actually return purification plant and generic time series

# fhpredict 0.13.0 (2020-04-10)

* Let predict_quality() not delete all existing predictions before adding the 
  new predictions. Instead, update predictions that already exist in the 
  database with PUT and add only those new predictions with POST that are not 
  yet in the database.
* Remove arg "user_id" from determine_days_to_predict() -> do not read 
  prediction period from a special bathing spot "Vorhersagedatum".
* Avoid date duplicates by calculating daily means within predict_quality()
* Create only four plots (remove "Modellimplikationen")

# fhpredict 0.12.0 (2020-03-24)

* Do not filter time series' for the summer period
* Add new (private) function get_model_overview()
* Add argument "import" to predict_quality() -> allow to skip rain data import

# fhpredict 0.11.1 (2020-03-19)

* Pin versions of KWB packages in remotes section of DESCRIPTION file

# fhpredict 0.11.0 (2020-03-11)

* Use from = "yesterday", to = "tomorrow" as defaults in predict_quality()

# fhpredict 0.10.1 (2020-03-10)

* Fix bug in predict_quality(): pass the actual user ID

# fhpredict 0.9.1 (2019-11-21)

* Improve error handling when getting and testing a token
* Stop if Radolan URLs could not be retrieved
* Change names of Environment variables closing #21

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
* remove invalid e.coli values
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
