# calculate_daily_means --------------------------------------------------------
calculate_daily_means <- function(df, date_column = "datum")
{
  df[[date_column]] <- as.Date(
    format(kwb.utils::selectColumns(df, date_column), "%Y-%m-%d")
  )

  df %>%
    kwb.utils::hsMatrixToListForm(keyFields = date_column) %>%
    dplyr::group_by(.data$datum, .data$parName) %>%
    dplyr::summarise(parVal = mean(parVal)) %>%
    tidyr::pivot_wider(names_from = "parName", values_from = "parVal") %>%
    as.data.frame()
}
