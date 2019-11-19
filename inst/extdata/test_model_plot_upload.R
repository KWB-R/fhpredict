if (FALSE)
{
  user_id <- 4
  spot_id <- 62

  # What models do exist?
  (model_ids <- unique(fhpredict::api_get_model(user_id, spot_id)$id))

  # Delete all models
  for (model_id in model_ids) {
    fhpredict::api_delete_model(user_id, spot_id, model_id)
  }

  model_id <- 70

  # What plots do already exist?
  fhpredict:::show_model_plots(user_id, spot_id, model_id)

  model_plots <- create_model_plots()

  model <- "My model"

  model_id <- fhpredict::api_add_model(user_id, spot_id, model, parameter = "conc_ec")

  #model_plot <- model_plots[[1]]

  # Upload some example plots
  get <- kwb.utils::selectElements

  for (model_plot in model_plots) {

    fhpredict:::upload_model_plot(
      user_id, spot_id, model_id,
      plot_file = fhpredict:::plot_to_svg(get(model_plot, "expr")),
      title = get(model_plot, "title"),
      description = get(model_plot, "description")
    )
  }

  fhpredict:::upload_model_plot(
    user_id, spot_id, model_id,
    plot_file = plot_to_svg(barplot(1:3)),
    title = "Three bars",
    description = "One, two, three"
  )

  fhpredict:::upload_model_plot(
    user_id, spot_id, model_id,
    plot_file = plot_to_svg(plot(rnorm(100), rnorm(100))),
    title = "So random",
    description = "Random numbers versus random numbers"
  )

  # Show the plots again
  fhpredict:::show_model_plots(user_id, spot_id, model_id)

  # Delete the model
  fhpredict::api_delete_model(user_id, spot_id, model_id)
}

# create_model_plots -----------------------------------------------------------
create_model_plots <- function(model)
{
  list(
    list(
      expr = {
        barplot(1:3)
      },
      title = "Three bars",
      description = "One, two, three"
    ),

    list(
      expr = {
        plot(rnorm(100), rnorm(100))
      },
      title = "So random",
      description = "Random numbers versus random numbers"
    )
  )
}
