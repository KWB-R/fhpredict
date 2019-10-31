# plot_effects -----------------------------------------------------------------

#' Modellimplikationen
#'
#' @param model model object as returned by \code{\link{api_get_model}}
#'
plot_effects <- function(model)
{
  #model <- fhpredict::api_get_model(3, 67, 90)
  data <- kwb.utils::selectElements(model, "data")

  # Function to get statistics for each column except the first (e.coli)
  get_stats <- function(FUN) {
    as.data.frame(t(apply(data[, -1, drop = FALSE], MARGIN = 2, FUN = FUN)))
  }

  means <- get_stats(mean)
  sds <- get_stats(stats::sd)

  Mean_df <- as.data.frame(matrix(
    rep(means, each = nrow(data)), nrow = nrow(data), ncol = length(means)
  ))

  colnames(Mean_df) <- names(means)

  Mean_df <- as.data.frame(apply(Mean_df, 2, as.numeric))

  sequences <- as.data.frame(apply(data, 2, function(x) {
    seq(0, mean(x) + 3 * stats::sd(x), length.out = length(x))
  }))[2:ncol(data)]

  counterfactual <- list()

  for (column in colnames(sequences)) {
    marginal <- sequences[, column, drop = FALSE]
    control <- Mean_df %>% dplyr::select(which(colnames(Mean_df) != column))
    counterfactual[[column]] <- dplyr::bind_cols(marginal, control)
    counterfactual[[column]]["variable"] <- column
    counterfactual[[column]]["Wert"] <- marginal
  }

  newdata <- dplyr::bind_rows(counterfactual)

  pp <- as.data.frame(t(apply(
    X = rstanarm::posterior_predict(model, newdata = newdata),
    MARGIN = 2,
    FUN = stats::quantile,
    probs = c(.025, 0.5, 0.975)
  )))

  pred <- dplyr::bind_cols(pp, newdata)

  ggplot2::ggplot(pred, ggplot2::aes(
    x = .data$Wert,
    y = .data[["50%"]],
    ymin = .data[["2.5%"]],
    ymax = .data[["97.5%"]]
  )) +
    ggplot2::geom_ribbon(alpha = 0.4) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(. ~ variable, scales = "free_x") +
    ggplot2::xlab("Wert") +
    ggplot2::ylab("Percentil")
}
