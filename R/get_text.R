# get_text ---------------------------------------------------------------------
get_text <- function(keyword, ...)
{
  texts <- kwb.utils::resolve(..., x = list(
    converting_time = "Converting time columns from text to POSIXct",
    could_not_build_model = "Could not create a valid model!",
    deleting_data_points = "Deleting <n> <subject> data points",
    filtering_variables = paste0(
      "Filtering <n> variables for those matching '<pattern>'"
    ),
    getting_radolan_urls_between = paste0(
      "Getting URLs to Radolan files between <from> and <to>"
    ),
    getting_radolan_urls = paste0(
      "Getting URLs to Radolan files for year <year><season_info>"
    ),
    info_provide_rain_data = paste0(
      "Please use the returned object in a loop to perform the actual data ",
      "import,\nas in the following code:\n\n",
      "control <- provide_rain_data(<user_id>, <spot_id>)\n\n",
      "while (control$remaining > 0) {\n",
      "  control <- provide_rain_data(control = control)\n",
      "}"
    ),
    inserting_rain = "Inserting <n> rain data records into the database",
    invalid_token = "The token is not valid. The response was:",
    invalid_user = "Invalid user_id: <user_id>. See above for possible users.",
    max_steps_is_zero = paste0(
      "max_steps = 0 in stepwise() -> Not enough data points available!"
    ),
    model_created = "Model created on <datetime> with fhpredict::build_model()",
    model_deleted = "The model with id <model_id> (<comment>) was deleted.",
    model_found = paste0(
      "A model was found and saved (model_id = <model_id>):\n<formula>"
    ),
    model_stored = paste0(
      "The model has been stored in the database. ",
      "It has been given the id <model_id>."
    ),
    model_with_id = "model with id = <model_id>",
    no_data = paste0(
      "No <subject> data available for <spot_ident>.\n",
      "Nothing to delete."
    ),
    no_measurement_dates = "No measurement dates.",
    no_measurement_dates_in_range = paste0(
      "No measurement dates between <from> and <to>."
    ),
    no_measurement_dates_in_season = "No measurement dates in bathing season.",
    no_measurements = paste0(
      "No (microbiological) measurements stored for <spot_ident>."
    ),
    no_model_file = paste0(
      "No model file has yet been uploaded for model_id = <model_id>."
    ),
    no_models_stored = "There are no models stored for <spot_ident>.",
    no_rain_data = "No rain data stored for <spot_ident>.",
    no_such_model = paste0(
      "There is no model with id <model_id> stored for <spot_ident>.\n",
      "Call api_get_model(user_id = <user_id>, spot_id = <spot_id>) ",
      "for available models."
    ),
    no_write_permission = paste0(
      "No permission to write to file '<file>'\n",
      "Permissions/user/group of folder '<folder>':\n",
      "  <permissions>/<user>/<group>\n",
      "Error message: <error>"
    ),
    not_one_model = "Not exactly one model was returned as expected!",
    nothing_to_add = "Nothing to add.",
    process_returned_no_data = paste0(
      "kwb.flusshygiene::process_model_riverdata() returned an empty data ",
      "frame! See the structure of 'riverdata' above."
    ),
    rain_added = "<n> rain data records have been inserted to the database.",
    reading_data = "Reading <subject> data from database",
    reading_model = "Reading <what> from the database",
    reading_token = "Reading access token from '<file>'",
    removing_data_frame = paste0(
      "Removing element '<element>' from list of data frames"
    ),
    request_token_failed = paste0(
      "Request for token failed. Status: <status>. Returning NULL."
    ),
    requesting_token = "Requesting a new access token",
    river_data_not_supported = paste0(
      "The argument 'river_data' is not supported any more. Please use the ",
      "new argument 'spot_data' to pass a list of data frames related to ONE ",
      "bathing spot ONLY."
    ),
    river_not_supported = paste0(
      "The argument 'river' is not supported any more. You may use the ",
      "new argument 'prefix' to prefix the names of the returned models."
    ),
    rmodel_deprecated = "Deprecated. Binary model file has been uploaded.",
    sorting_by_time = "Sorting data frame by time",
    spot_data_expected_type = paste0(
      "The argument 'spot_data' is expected to be a list of data frames!"
    ),
    step_failed = paste0(
      "stat::step() failed for the following step numbers:\n<details>"
    ),
    stored_for_spot = "stored for <spot_ident>",
    spot_ident = "user_id = <user_id> and spot_id = <spot_id>",
    using_variables = "Using <n> variables:\n- <varlist>\n",
    writing_token = "Writing access token to '<file>'"
  ))

  text <- texts[[keyword]]

  if (is.null(text)) {
    clean_stop(sprintf(
      "No text stored for keyword '%s'. Consider extending get_text().",
      keyword
    ))
  }

  text
}
