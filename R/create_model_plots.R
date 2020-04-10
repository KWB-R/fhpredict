# create_model_plots -----------------------------------------------------------
create_model_plots <- function(tests, model)
{
  # Name of reference model
  ref_model <- kwb.utils::selectColumns(tests, "model")[1]

  # Create the data overview plot
  data_overview_plot <- plot_data_overview(model)

  # Read the rain summary from the attribute "rain_summary"
  rain_summary <- kwb.utils::getAttribute(data_overview_plot, "rain_summary")

  create_plot <- function(gg_plot, title, description) {
    list(
      gg_plot = gg_plot,
      title = title,
      description = description
    )
  }

  list(
    # Plot 1
    create_plot(
      gg_plot = plot_model_calibration(tests, ref_model),
      title = "\u00dcberblick \u00fcber getestete Modelle",
      description = paste(
        "Auswertung der Teststatistiken und das Bestimmtheitsma\u00df.",
        "Diese Funktion stellt die getesteten Modelle grafisch dar."
      )
    ),
    # Plot 2
    create_plot(
      gg_plot = data_overview_plot,
      title = "Datenqualit\u00e4t und Kalibrationsbereich",
      description = paste(
        "Diese Abbildung zeigt die Anzahl Regenassoziierter Datenpunkte.",
        "Es sind die vom Modell verwendeten Regenvariablen dargestellt.",
        "Der Anteil der Tage mit Regen > 5 mm im Vorfeld der Messung",
        "betr\u00e4gt", max(rain_summary$Anteil_regen), "%."
      )
    ),
    # Plot 3
    create_plot(
      gg_plot = plot_fit_vs_data(model),
      title = "Modelbewertung gegen Messwerte",
      description = paste(
        "Die Grafik stellt die vom Modell vorhergesagten",
        "Konzentrationsbereiche den tats\u00e4chlichen Messwerten",
        "gegen\u00fcber."
      )
    ),
    # Plot 4
    create_plot(
      gg_plot = plot_contigency(model),
      title = "Perzentile auf Basis klassifizierter Datenpunkte",
      description = paste(
        "Berechnete Perzentile (y-Achse) sollten mit Klassifizierung x-Achse ",
        "\u00fcbereinstimmen"
      )
    )
    # # Plot 5
    # , create_plot(
    #   gg_plot = plot_effects(model),
    #   title = "Modellimplikationen",
    #   description = paste(
    #     "Das sind die Modellimplikationen..."
    #   )
    # )
    # # Plot 6
    # , create_plot(
    #   gg_plot = empty_ggplot("Leere Grafik."),
    #   title = "Grafik 6: Noch nicht vergeben",
    #   description = paste(
    #     "Keine Grafik - keine Beschreibung."
    #   )
    # )
  )
}
