# prepare_river_data -----------------------------------------------------------
prepare_river_data <- function(river_list)
{
  river_ts <- calc_t(datalist = river_list)

  # Names of list elements
  elements <- names(river_ts)

  is_discharge <- grepl("^q",  elements)
  is_treatment <- grepl("^ka", elements)
  is_i         <- grepl("^i",  elements)
  is_rain      <- grepl("^r",  elements)

  river_ts[is_discharge] <- lapply(river_ts[is_discharge], add_meancol)
  river_ts[is_treatment] <- lapply(river_ts[is_treatment], add_meancol)
  river_ts[is_i]         <- lapply(river_ts[is_i],         add_meancol)
  river_ts[is_rain]      <- lapply(river_ts[is_rain],      add_meancol)

  river_ts
}

# calc_t -----------------------------------------------------------------------
calc_t <- function(datalist)
{
  # Filter for summer months in the hygienic data
  hygiene_element <- grep("hygiene", names(datalist), value = TRUE)
  stopifnot(length(hygiene_element) == 1)

  hygienic <- kwb.utils::selectElements(datalist, hygiene_element)

  # Data frames with non-hygienic data
  non_hygienics <- datalist[setdiff(names(datalist), hygiene_element)]

  # Filter hygienic measurements for months in summer (May to September)
  hygienic_summer <- filter_for_months(hygienic, 5:9)

  # Filter non-hygienic measurements for months in summer (April to September)
  non_hygienics_summer <- lapply(non_hygienics, filter_for_months, 4:9)

  # z-transform the data frames with non-hygienic data
  non_hygienics_z <- lapply(non_hygienics_summer, transform_z)

  # Recompose the list of hygienic and non-hygienic data and set original names
  stats::setNames(c(list(hygienic_summer), non_hygienics_z), names(datalist))
}

# filter_for_months: filter for month numbers ----------------------------------
filter_for_months <- function(df, month_numbers)
{
  dates <- kwb.utils::selectColumns(df, "datum")

  df[lubridate::month(dates) %in% month_numbers, ]
}

# transform_z ------------------------------------------------------------------
transform_z <- function(df)
{
  # Are the columns rain columns?
  is_rain_column <- grepl("^r_.*", names(df))

  # Transform rain columns: log-transformed and 1/sigma2 (?)
  df[is_rain_column] <- lapply(df[is_rain_column], function(x) log(x + 1))

  # Return the data frame with rain columns being transformed
  df
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
