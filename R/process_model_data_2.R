#kwb.utils::assignPackageObjects("fhpredict")
# Load some testdata
#data_dir <- system.file("extdata/testdata", package = "fhpredict")
#file <- dir(data_dir, "RData$", full.names = TRUE)[1]
#spot_data <- kwb.utils::loadObject(file, "spot_data")
#riverdata <- prepare_river_data(spot_data)
#variables <- c("r_radolan", "r_radolan_abs_1", "r_radolan_abs_2", "r_radolan_abs_3", "r_radolan_abs_4", "r_radolan_abs_5", "r_radolan_mean_12", "r_radolan_mean_123", "r_radolan_mean_1234", "r_radolan_mean_12345", "r_radolan_mean_2345", "r_radolan_mean_345", "r_radolan_mean_45", "r_radolan_mean_234", "r_radolan_mean_23", "r_radolan_mean_34", "r_mean", "r_mean_abs_1", "r_mean_abs_2", "r_mean_abs_3", "r_mean_abs_4", "r_mean_abs_5", "r_mean_mean_12", "r_mean_mean_123", "r_mean_mean_1234", "r_mean_mean_12345", "r_mean_mean_2345", "r_mean_mean_345", "r_mean_mean_45", "r_mean_mean_234", "r_mean_mean_23", "r_mean_mean_34")

# process_model_data_2 ---------------------------------------------------------
process_model_data_2 <- function (riverdata, variables)
{
  # Do not consider variables having a colon in their name
  variables <- variables[! grepl(":", variables)]

  # Find the index containing the hygiene data
  is_hygiene <- grepl("^hygiene", names(riverdata))

  # There must be exactly one data frame with hygiene data
  stopifnot(sum(is_hygiene) == 1)

  # Extract the hygiene data (only columns "datum" and "e.coli") from the list
  columns <- c("datum", "e.coli")
  hygiene <- kwb.utils::selectColumns(riverdata[[which(is_hygiene)]], columns)

  # Provide all other data frames in a new list "typedata"
  typedata <- riverdata[! is_hygiene]

  # If the first variable needs log-transformation, log-transform the e.coli
  # values and prefix the corresponding column with "log_"
  if (is_log_var(variables[1])) {
    hygiene <- apply_at_index(hygiene, 2, log10, prefix = "log_")
  }

  # Are variables in the other data frames log-transformed?
  is_log <- needs_log(elements = names(typedata), vars = variables[-1])

  # In each non-hygienic data frame that is to be log-transformed, log-transform
  # all but the first column (which is the date)
  typedata <- apply_at_index(typedata, is_log, prefix = "", function(x) {

    # Calculate safe_log10 of each but the first column
    apply_at_index(x, -1L, safe_log10, prefix = "log_")
  })

  # Extend data frames in typedata with additional variables of which only
  # the requested variables are kept. If a data frame does not contain any
  # of the requested variables, skip the data frame (by excluding NULL)
  typedata_big <- kwb.utils::excludeNULL(lapply(typedata, function(df) {
    #df <- typedata[[1]]

    df <- kwb.flusshygiene::unroll_lagdays(df)

    if (length(common <- intersect(names(df), variables))) {

      kwb.utils::selectColumns(df, c("datum", common))
    }
  }))

  # Create list of data frames
  data_frames <- c(list(hygiene = hygiene), typedata_big)

  # Merge all data frames in model_list by column "datum"
  merge_data_frames(data_frames, by = "datum", all = TRUE)
}

# needs_log --------------------------------------------------------------------
needs_log <- function(elements, vars, dbg = TRUE)
{
  info_vars <- kwb.utils::noFactorDataFrame(
    var = vars,
    type = extract_variable_type(vars),
    log = is_log_var(vars)
  )

  info_sets <- kwb.utils::noFactorDataFrame(
    set = elements,
    type = extract_data_type(elements)
  )

  kwb.utils::printIf(dbg, info_vars)
  kwb.utils::printIf(dbg, info_sets)

  info_sets$type %in% unique(info_vars$type[info_vars$log])
}

# extract_variable_type --------------------------------------------------------
extract_variable_type <- function(x)
{
  sub("^(log_)?([a-z]{1,3})_.*", "\\2", x)
}

# extract_data_type ------------------------------------------------------------
extract_data_type <- function(x)
{
  sub("^([a-z]{1,3})_.*", "\\1", x)
}

# is_log_var -------------------------------------------------------------------
is_log_var <- function(x)
{
  grepl("^log", x)
}
