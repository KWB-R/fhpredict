# prepare_river_data -----------------------------------------------------------
prepare_river_data <- function(river_list)
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

      df <- df %>%
        filter_for_months(5:9)

    } else if (grepl("^(q|ka|i|r)_", element)) {

      df <- df %>%
        filter_for_months(4:9) %>%
        add_meancol()

    } else {

      stop("Unexpected element in river_list: ", element)
    }

    # z-transform the rain data columns
    if (grepl("^r_", element)) {

      # Are the columns rain data columns?
      is_rain <- grepl("^r_.*", names(df))

      # Transform rain columns: log-transformed and 1/sigma2 (?)
      df[is_rain] <- lapply(df[is_rain], function(x) log(x + 1))
    }

    # Copy the transformed data frame back into the list
    river_list[[element]] <- df
  }

  river_list
}

# remove_hygiene_data ----------------------------------------------------------
remove_hygiene_data <- function(datalist)
{
  stopifnot(is_river_data_element(datalist))

  hygiene_element <- grep("^hygiene", names(datalist), value = TRUE)

  result <- kwb.utils::catAndRun(
    sprintf("Removing element '%s' from list of data frames", hygiene_element),
    datalist[setdiff(names(datalist), hygiene_element)]
  )

  result
}

# filter_for_months: filter for month numbers ----------------------------------
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
