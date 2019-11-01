# plot_data_overview -----------------------------------------------------------

#' Abbildung Datenqualität und Kalibrationsbereich
#'
#' Diese Abbildung zeigt die Anzahl Regenassoziierter Datenpunkte
#'
#' @param model model object as returned by \code{\link{api_get_model}}
#'
plot_data_overview <- function(model)
{
  #model <- fhpredict::api_get_model(3, 53, 77)
  #`%>%` <- magrittr::`%>%`

  model_data <- kwb.utils::selectElements(model, "data")

  rain <- model_data[, grepl("r_", names(model_data)), drop = FALSE]

  rain_long <-tidyr::gather(rain, "Regen", "Wert")

  rescale <- function(x) {
    exp(x) - 1
  }

  round_rescale <- function(x, fun = identity) {
    round(fun(exp(x))) - 1
  }

  rain_summary <- rain_long %>%
    dplyr::group_by(.data$Regen) %>%
    dplyr::summarise(
      min = round_rescale(.data$Wert, min),
      max = round_rescale(.data$Wert, max),
      MW = round_rescale(.data$Wert, mean),
      Tage = dplyr::n(),
      Regentage = sum(rescale(.data$Wert) > 5),
      Anteil_regen = round(kwb.utils::percentage(.data$Regentage, .data$Tage))
    )

  Anteil_Regentage <- 100 * round(
    sum(round(rescale(rain_long$Wert)) > 5) / rain_summary$Regentage,
    digits = 2
  )

  n_bins <- nrow(rain_long) / 5

  gg <- ggplot2::ggplot(rain_long, ggplot2::aes(x = rescale(.data$Wert))) +
    ggplot2::geom_histogram(bins = n_bins, fill = "steelblue", col = "black") +
    ggplot2::facet_wrap( . ~ Regen) +
    ggplot2::xlab("Niederschlagsdaten [mm]") +
    ggplot2::theme_bw(base_size = 16) +
    ggplot2::ylab("Anzahl Messungen") +
    ggplot2::geom_label(
      data = rain_summary,
      ggplot2::aes(
        x = max(.data$max) / 2,
        y = n_bins,
        group = .data$Regen,
        label = sprintf("Obere Kalibrationsgrenze = %0.1f mm", .data$max)
      )
    ) # +
    # ggplot2::ggtitle(
    #   "Genutzte Regenvariablen",
    #   subtitle = sprintf(
    #     "Anteil der Tage mit Regen > 5 mm im Vorfeld der Messung = %f %%",
    #     max(rain_summary$Anteil_regen)
    #   )
    # )

  structure(gg, rain_summary = rain_summary)
}
