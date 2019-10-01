# build_model ------------------------------------------------------------------

#' Build Water Quality Model for Bathing Spot
#'
#' @param user_id user ID
#' @param spot_id bathing spot ID
#' @param seed if \code{TRUE} the random number generate is initialised using
#'   this value in a call to \code{\link{set.seed}}
#' @return list with elements \code{data}, \code{success}, \code{message}
#' @export
build_model <- function(user_id, spot_id, seed = NULL)
{
  #kwb.utils::assignPackageObjects("fhpredict")
  #user_id=5;spot_id=41;seed=NULL

  # Get data in the format that is required by build_and_validate_model()
  spot_data <- try(provide_input_data(user_id, spot_id))

  if (inherits(spot_data, "try-error")) {

    return(create_result(success = FALSE, message = as.character(spot_data)))
  }

  # Initialise the random number generator if a seed is given
  if (! is.null(seed)) {

    stopifnot(is.numeric(seed))
    set.seed(seed)
  }

  result <- build_and_validate_model(spot_data = spot_data)

  if (length(result) == 0) {

    return(create_result(
      success = FALSE, message = "Could not create a valid model!"
    ))
  }

  model <- kwb.utils::selectElements(result, "stanfit")

  api_add_model(
    user_id = user_id,
    spot_id = spot_id,
    model = model,
    comment = sprintf(
      "Model created on %s with fhpredict::build_model()", Sys.time()
    )
  )

  create_result(success = TRUE)
}
