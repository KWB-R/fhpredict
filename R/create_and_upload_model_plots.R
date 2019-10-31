# create_and_upload_model_plots ------------------------------------------------
create_and_upload_model_plots <- function(
  tests, model, user_id, spot_id, model_id
)
{
  #tests = kwb.utils::selectElements(result, "stat_tests")

  # Name of reference model
  ref_model <- kwb.utils::selectColumns(tests, "model")[1]

  # Create the calibration plot
  calibration_plot <- plot_model_calibration(tests, ref_model)

  # Create the data overview plot
  data_overview_plot <- plot_data_overview(model)

  # Create the fit versus data plot
  fit_vs_data_plot <- plot_fit_vs_data(model)

  upload_model_plot(
    user_id, spot_id, model_id,
    plot_file = plot_to_svg(calibration_plot),
    title = "\u00dcberblick \u00fcber getestete Modelle",
    description = paste(
      "Diese Funktion stellt die getesteten Modelle grafisch dar."
    )
  )

  upload_model_plot(
    user_id, spot_id, model_id,
    plot_file = plot_to_svg(data_overview_plot),
    title = "Datenqualit\u00e4t und Kalibrationsbereich",
    description = paste(
      "Diese Abbildung zeigt die Anzahl Regenassoziierter Datenpunkte"
    )
  )

  upload_model_plot(
    user_id, spot_id, model_id,
    plot_file = plot_to_svg(fit_vs_data_plot),
    title = "Modelbewertung gegen Messwerte",
    description = paste(
      "Die Grafik stellt die vom Modell vorhergesagten",
      "Konzentrationsbereiche den tats\u00e4chlichen Messwerten gegen\u00fcber."
    )
  )
}
