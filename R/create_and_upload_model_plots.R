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

  # Read the rain summary from the attribute "rain_summary"
  rain_summary <- kwb.utils::getAttribute(data_overview_plot, "rain_summary")

  # Create the fit versus data plot
  fit_vs_data_plot <- plot_fit_vs_data(model)

  # Create contigency plot
  contigency_plot <- plot_contigency(model)

  # Upload Plot 1: Calibration plot
  upload_model_plot(
    user_id, spot_id, model_id,
    plot_file = plot_to_svg(calibration_plot),
    title = "\u00dcberblick \u00fcber getestete Modelle",
    description = paste(
      "Auswertung der Teststatistiken und das Bestimmtheitsma\u00df.",
      "Diese Funktion stellt die getesteten Modelle grafisch dar."
    )
  )

  # Upload Plot 2: Overview plot
  upload_model_plot(
    user_id, spot_id, model_id,
    plot_file = plot_to_svg(data_overview_plot),
    title = "Datenqualit\u00e4t und Kalibrationsbereich",
    description = paste(
      "Diese Abbildung zeigt die Anzahl Regenassoziierter Datenpunkte.",
      "Es sind die vom Modell verwendeten Regenvariablen dargestellt.",
      "Der Anteil der Tage mit Regen > 5 mm im Vorfeld der Messung",
      "betr\u00e4gt", max(rain_summary$Anteil_regen), "%."
    )
  )

  # Upload Plot 3: Fit versus data plot
  upload_model_plot(
    user_id, spot_id, model_id,
    plot_file = plot_to_svg(fit_vs_data_plot),
    title = "Modelbewertung gegen Messwerte",
    description = paste(
      "Die Grafik stellt die vom Modell vorhergesagten",
      "Konzentrationsbereiche den tats\u00e4chlichen Messwerten gegen\u00fcber."
    )
  )

  # Upload Plot 4: Contigency plot
  upload_model_plot(
    user_id, spot_id, model_id,
    plot_file = plot_to_svg(contigency_plot),
    title = "Perzentile auf Basis klassifizierter Datenpunkte",
    description = paste(
      "Berechnete Perzentile (y-Achse) sollten mit Klassifizierung x-Achse ",
      "\u00fcbereinstimmen"
    )
  )

  empty_plot <- empty_ggplot("Leere Grafik.")

  # Upload plot 5: Not yet specified
  upload_model_plot(
    user_id, spot_id, model_id,
    plot_file = plot_to_svg(empty_plot),
    title = "Grafik 5: Noch nicht vergeben",
    description = paste(
      "Keine Grafik - keine Beschreibung."
    )
  )

  # Upload plot 6: Not yet specified
  upload_model_plot(
    user_id, spot_id, model_id,
    plot_file = plot_to_svg(empty_plot),
    title = "Grafik 6: Noch nicht vergeben",
    description = paste(
      "Keine Grafik - keine Beschreibung."
    )
  )
}
