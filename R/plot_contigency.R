# plot_contigency --------------------------------------------------------------

#' Model Classification Versus Percentiles
#'
#' Abbildung zeigt durch Model klassifizierte Werte im Vergelich zu den auf
#' diesen Werten berechneten Perzentilgrenzen
#'
#' @param model model object as returned by \code{\link{api_get_model}}
#'
plot_contigency <- function(model)
{
  #model <- fhpredict::api_get_model(3, 67, 78)
  #kwb.utils::assignPackageObjects("fhpredict")
  #`%>%` <- magrittr::`%>%`

  percentiles <- simulate_classification(model)

  percentiles$observations <- kwb.utils::selectElements(model, "y")

  # Conversion function
  convert <- function(x, a) {
    10^(mean(x) + a * stats::sd(x))
  }

  assessment <- percentiles %>%
    dplyr::group_by(.data$Bewertung) %>%
    dplyr::summarise(
      P90  = convert(.data$observations, a = 1.282),
      P95 = convert(.data$observations, a = 1.65),
      N = dplyr::n()
    ) %>%
    tidyr::gather("Perzentil", "Wert", - .data$Bewertung, - .data$N)

  assessment$Bewertung <- factor(
    assessment$Bewertung,
    levels = c("ausgezeichnet", "gut", "ausreichend", "mangelhaft"),
    ordered = TRUE
  )

  thresholds <- log10(c(steelblue = 500, orange = 1000, red3 = 900))

  n_thresholds <- length(thresholds)

  ggplot2::ggplot(assessment, ggplot2::aes(
    x = .data$Bewertung,
    y = log10(.data$Wert),
    fill = .data$Perzentil)
  ) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::geom_hline(
      yintercept = thresholds,
      lwd = rep(1, n_thresholds),
      lty = rep(2, n_thresholds),
      col = names(thresholds)
    ) +
    ggplot2::scale_fill_grey() +
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::ylab("Perzentile") +
    # ggplot2::ggtitle(
    #   label = "Perzentile auf Basis klassifizierter Datenpunkte",
    #   subtitle = "Berechnete Perzentile (y-Achse) sollten mit Klassifizierung x-Achse uebereinstimmen"
    # ) +
    ggplot2::geom_label(
      ggplot2::aes(x = .data$Bewertung, y = 3.2, label = paste("N =", .data$N)),
      fill = "skyblue", alpha = 0.5, label.size = NA
    )
}
