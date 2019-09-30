# Test build_and_validate_model() ----------------------------------------------
if (FALSE)
{
  # Get data in the format that is required by build_and_validate_model()
  spot_data <- fhpredict::provide_input_data(user_id, spot_id)

  # Remove empty data frames
  #spot_data <- spot_data[lengths(spot_data) > 0]

  #reset_time <- function(x) as.POSIXct(substr(as.character(x), 1, 10))

  #spot_data$hygiene$datum <- reset_time(spot_data$hygiene$datum)
  #spot_data$r$datum <- reset_time(spot_data$r$datum)

  result <- fhpredict:::build_and_validate_model(
    spot_data = spot_data,
    prefix = "spot18_"
  )

  fhpredict::api_add_model(user_id, spot_id, result$stanfit, comment = "great!")

  object.size(result[[3]])
  rstanarm::launch_shinystan(result[[3]])


  dn <- data.frame(r_mean_mean_23 = seq(0, 40, .5))

  pp <- apply(rstanarm::posterior_predict(result[[3]], newdata = dn), 2,
              quantile, probs = c(0.025, 0.5, 0.975))

  result[[3]]$model

  plot(dn$r_mean_mean_23, pp[2,])
  lines(dn$r_mean_mean_23, pp[1,])
  lines(dn$r_mean_mean_23, pp[3,])
}
