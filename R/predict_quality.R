# predict_quality --------------------------------------------------------------

#' Predict Water Quality for Bathing Spot Using Stored Model
#'
#' @param user_id user ID
#' @param spot_id bathing spot ID
#' @param date Date object or date string in format yyyy-mm-dd
#' @return list with elements \code{data}, \code{success}, \code{message}
#' @export
predict_quality <- function(user_id, spot_id, date = Sys.Date())
{
  #kwb.utils::assignPackageObjects("fhpredict")
  #user_id=1;spot_id=1
  #user_id=5;spot_id=41

  # Get available models
  models <- try(api_get_model(user_id, spot_id))

  if ((failed <- inherits(model, "try-error")) || nrow(models) == 0) {

    return(create_result(
      success = FALSE,
      message = if (failed) {
        as.character(model)
      } else {
        sprintf(
          "No models stored for user_id = %d, spot_id = %d", user_id, spot_id
        )
      }
    ))
  }

  model_id <- kwb.utils::selectElements(models, "id")[nrow(models)]

  model <- try(api_get_model(user_id, spot_id, model_id))

  if (inherits(model, "try-error")) {

    return(create_result(
      success = FALSE,
      message = as.character(model)
    ))
  }

  path <- path_predictions(user_id, spot_id)

  #str(postgres_get(path)$data)

  result <- postgres_post(path, body = list(
    date = date,
    prediction = "gut"
  ))

  if (length(result) == 0) {

    response <- kwb.utils::getAttribute(result, "response")

    return(create_result(
      success = FALSE,
      message = httr::content(response)$error$message
    ))
  }

  return(create_result(
    success = result$success,
    message = result$message
  ))
}
