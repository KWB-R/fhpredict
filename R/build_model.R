# build_model ------------------------------------------------------------------

#' Build Water Quality Model for Bathing Spot
#'
#' @param user_id user ID
#' @param spot_id bathing spot ID
#' @param seed if \code{TRUE} the random number generate is initialised using
#'   this value in a call to \code{\link{set.seed}}
#' @param delete_rain if \code{TRUE}, all rain data are deleted after successful
#'   model creation. This was used to avoid long loading times (freezing) in the
#'   frontend but should not be necessary any more! The default is \code{FALSE}.
#' @return list with elements \code{data}, \code{success}, \code{message}
#' @export
build_model <- function(user_id, spot_id, seed = NULL, delete_rain = FALSE)
{
  #kwb.utils::assignPackageObjects("fhpredict")
  #user_id=3;spot_id=43;seed=NULL

  # Get data in the format that is required by build_and_validate_model()
  spot_data <- try(provide_input_data(user_id, spot_id))

  if (is_error(spot_data)) {
    return(create_failure(spot_data))
  }

  result <- try({

    # Exclude measurements with NA in column e.coli
    spot_data[[1]] <- remove_missing_ecoli(hygiene = spot_data[[1]])

    # Initialise the random number generator if a seed is given
    if (! is.null(seed)) {
      stopifnot(is.numeric(seed))
      set.seed(seed)
    }

    # Build and validate a model from the data
    build_and_validate_model(spot_data = spot_data)
  })

  if (is_error(result)) {
    return(create_failure(result))
  }

  if (length(result) == 0) {
    return(create_result(
      success = FALSE, message = get_text("could_not_build_model")
    ))
  }

  result <- try({

    model <- kwb.utils::selectElements(result, "stanfit")

    formula <- utils::capture.output(print(model$formula))[1]

    comment <- jsonlite::toJSON(auto_unbox = TRUE, c(
      list(formula = formula),
      kwb.utils::removeColumns(result$sorted_models[1, ], c("river", "model"))
    ))

    parameter <- "conc_ec"

    # Provide the data frame containing the results of the statistical tests
    stat_tests <- kwb.utils::selectElements(result, "stat_tests")

    model_id <- api_add_model(
      user_id = user_id,
      spot_id = spot_id,
      model = structure(model, stat_tests = stat_tests),
      comment = comment,
      parameter = parameter
    )

    # Create the plots describing the model, with titles and descriptions
    model_plots <- create_model_plots(stat_tests, model)

    # Upload Plots to the database
    upload_model_plots(user_id, spot_id, model_id, model_plots)

    # Compose a description for the output of this function
    indicators <- get_model_quality_string(x = result$sorted_models[1, ])
    description <- sprintf("Model formula: %s (%s)", formula, indicators)
  })

  if (is_error(result)) {
    return(create_failure(result))
  }

  # Delete all rain data if requested
  if (delete_rain) {

    result <- try(fhpredict::api_delete_rain(user_id, spot_id))

    if (is_error(result)) {
      return(create_failure(result))
    }
  }

  # will respond in a rea
  create_result(success = TRUE, message = get_text(
    "model_found",
    model_id = model_id,
    description = description
  ))
}

# get_model_quality_string -----------------------------------------------------
get_model_quality_string <- function(x)
{
  stopifnot(is.data.frame(x))
  stopifnot(nrow(x) == 1)

  get <- kwb.utils::selectColumns

  sprintf(
    "n_obs: %d, N: %0.3f, BP: %0.3f, R2: %0.3f",
    get(x, "n_obs"), get(x, "N"), get(x, "BP"), get(x, "R2")
  )
}
