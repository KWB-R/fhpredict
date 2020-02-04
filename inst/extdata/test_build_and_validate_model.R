# Test build_and_validate_model() ----------------------------------------------
if (FALSE)
{
  # Show available users
  fhpredict::api_get_users()

  # Select a user (by their ID)
  user_id <- 9

  # Show available bathing spot IDs
  fhpredict::api_get_bathingspot(user_id)$id

  # Select a bathing spot (by its ID)
  spot_id <- 41

  # Get data in the format that is required by build_and_validate_model()
  spot_data <- fhpredict::provide_input_data(user_id, spot_id)

  set.seed(1)

  result <- fhpredict:::build_and_validate_model(
    spot_data = spot_data
    # , prefix = "spot18_"
  )

  model <- result$stanfit

  fhpredict::api_add_model(user_id, spot_id, model, comment = "great!")

  object.size(model)

  rstanarm::launch_shinystan(model)

  dn <- data.frame(r_mean_mean_123 = seq(0, 40, .5),
                   r_mean_abs_5 = 1)

  prediction <- rstanarm::posterior_predict(model, newdata = dn)

  pp <- apply(prediction, 2, quantile, probs = c(0.025, 0.5, 0.975))

  model$model

  plot(dn[[1]], pp[2, ])
  lines(dn[[1]], pp[1, ])
  lines(dn[[1]], pp[3, ])
}
