# plot_model_calibration -------------------------------------------------------

#' Ueberblick ueber getestete Modelle
#'
#' Diese Funktion stellt die getestetet Modelle grafisch dar.
#'
#' @param tests data frame similar to element \code{sorted_models} of the list
#'   returned by \code{fhpredict:::build_and_validate_model}
#' @param ref_model name of the selected model (expected to be contained in
#'   column \code{model} of \code{tests})
#'
plot_model_calibration <- function(tests, ref_model = "model_01")
{
  #results <- restore("result")
  #tests <- result$stat_tests
  #ref_model <- results[[1]]$model[1]

  get <- kwb.utils::selectColumns

  above_five_percent <- function(x) {
    values <- c("richtig", "falsch")
    factor(ifelse(x > 0.05, values[1], values[2]), levels = values)
  }

  used_or_tested <- function(x) {
    values <- c("benutzt", "getestet")
    factor(ifelse(x == ref_model, values[1], values[2]), levels = values)
  }

  tests$normality <- above_five_percent(get(tests, "N"))
  tests$equal_variance <- above_five_percent(get(tests, "BP"))
  tests$used <- used_or_tested(get(tests, "model"))

  tests_long <- tidyr::gather(
    tests,
    "test",
    "Ergebnis",
    - .data$model,
    - .data$N,
    - .data$BP,
    - .data$stat_correct,
    - .data$R2,
    - .data$in50,
    - .data$in95,
    - .data$below95,
    - .data$below90,
    - .data$n_obs,
    - .data$river,
    - .data$used
  )

  colour_values <- c("falsch" = "red3", "richtig" = "steelblue")

  aesthetics <- ggplot2::aes(
    x = .data$model,
    y = .data$R2,
    shape = .data$used,
    col = .data$Ergebnis
  )

  labeller <- ggplot2::labeller(test = c(
    normality = "Residuen normalverteilt?",
    equal_variance = "Residuen konstant?"
  ))

  ggplot2::ggplot(tests_long, aesthetics) +
    ggplot2::geom_point(size = 4) +
    ggplot2::coord_flip() +
    ggplot2::facet_grid(. ~ test, labeller = labeller) +
    ggplot2::xlab("")+
    ggplot2::scale_shape(name = "Benutzt")+
    ggplot2::scale_color_manual(values = colour_values,
                                name = "Teststatistik") +
    #ggplot2::ggtitle("Auswertung Teststatistiken und Bestimmtheitsma\u00df") +
    ggplot2::theme_bw(base_size = 16) +
    ggplot2::ylab(expression("Bestimmtheitsma\u00df R"^2))
}
