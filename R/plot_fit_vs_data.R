# plot_fit_vs_data -------------------------------------------------------------

#' Plot Modelfit gegen Messwerte
#'
#' Abbildung zeigt Modelfit gegen Messwerte und 1 zu 1 Winkelhalbierende
#'
#' @param model model object as returned by \code{\link{api_get_model}}
plot_fit_vs_data <- function(model)
{
  #model <- fhpredict::api_get_model(3, 67, 78)
  percentiles <- simulate_classification(model)

  ggplot2::ggplot(percentiles, ggplot2::aes(
    y = kwb.utils::selectElements(model, "y"),
    x = .data$median,
    ymin = .data$lower,
    ymax = .data$upper,
    col = .data$Bewertung
  )) +
    ggplot2::geom_linerange(col = "grey") +
    ggplot2::geom_point(size = 2, col = "black", alpha = 0.6) +
    ggplot2::theme_bw(base_size = 16) +
    #  scale_color_manual(values = c("gut" = "steelblue",
    #                                "mangelhaft" = "red3",
    #                                "ausreichend" = "orange",
    #                                "ausgezeichnet" ="forest green")) +
    ggplot2::ylab("Vorhergesagtes Interval") +
    #ggplot2::ggtitle("Modelbewertung gegen Messwerte") +
    ggplot2::xlab("Messwerte lg[MPN/100mL]") +
    ggplot2::geom_abline(slope = 1, intercept = 0, lty = 2)
}

# simulate_classification ------------------------------------------------------

#' Hilfsfunktion zum Vorhersagen generieren
#'
#' @param model model object as returned by \code{\link{api_get_model}}
simulate_classification <- function(model)
{
  prediction <- rstanarm::posterior_predict(model)

  probs <- c(0.025, 0.5, 0.975, 0.9, 0.95)

  percentiles_matrix <- apply(prediction, 2, stats::quantile, probs = probs)

  percentiles <- as.data.frame(t(percentiles_matrix))

  names(percentiles) <- c("lower", "median", "upper", "P90", "P95")

  percentiles$Bewertung <- get_quality_from_percentiles(
    percentiles, version = 2
  )

  percentiles
}
