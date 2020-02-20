# prepare_river_data -----------------------------------------------------------
prepare_river_data <- function(river_list, column_hygiene = "e.coli", sd = 2)
{
  stopifnot(is_river_data_element(river_list))

  # Modify the data frames contained in river_list according to their type:
  # - Filter hygienic data for the summer months (May to September)
  # - Filter non-hygienic data for the summer months (April to September)
  #   and add a mean column
  # - z-transform the rain data columns of the rain data frame

  for (element in names(river_list)) {

    # Get the original data frame from the list
    df <- river_list[[element]]

    if (grepl("^hygiene", element)) {

      df <- filter_for_months(df, 5:9)

      # Get vector of E. coli concentrations
      conc <- kwb.utils::selectColumns(df, column_hygiene)

      # Apply a random noise to the E. coli concentrations
      df[[column_hygiene]] <- conc + round(stats::rnorm(length(conc), 0, sd))

    } else if (grepl("^(q|ka|i|r)_", element)) {

      # z-transform the rain data columns
      if (grepl("^r_", element)) {

        # Are the columns rain data columns?
        is_rain <- grepl("^r_.*", names(df))

        # Transform rain columns: log-transformed and 1/sigma2 (?)
        df[is_rain] <- lapply(df[is_rain], function(x) log(x + 1))
      }

      df <- add_meancol(filter_for_months(df, 4:9))

    } else {

      stop("Unexpected element in river_list: ", element)
    }

    # Copy the transformed data frame back into the list
    river_list[[element]] <- df
  }

  # Remove empty data frames
  remove_empty_data_frames(river_list)
}

# filter_for_months ------------------------------------------------------------
filter_for_months <- function(df, month_numbers)
{
  dates <- kwb.utils::selectColumns(df, "datum")

  df[lubridate::month(dates) %in% month_numbers, ]
}

# add_meancol ------------------------------------------------------------------
add_meancol <- function(df)
{
  # for rain and i #edit: + ka #2ndedit: + q
  for (prefix in get_value_column_prefixes(df)) {

    values <- df[, startsWith(names(df), prefix), drop = FALSE]

    df[, paste0(prefix,"_mean")] <- rowMeans(values, na.rm = TRUE)
  }

  df
}

# get_value_column_prefixes ----------------------------------------------------
get_value_column_prefixes <- function(df)
{
  unique(sub("([a-z])_.*", "\\1", names(df)[-1]))
}

# add_sumcol -------------------------------------------------------------------
add_sumcol <- function (df)
{
  # originally for ka, but not used
  prefix <- get_value_column_prefixes(df)

  if (length(df) > 2) {

    values <- df[, -1, drop = FALSE]

    df[[paste0(prefix, "_sum")]] <- rowSums(values, na.rm = TRUE)
  }

  df
}
