# all_elements_are_data_frames -------------------------------------------------
all_elements_are_data_frames <- function(x)
{
  stopifnot(is.list(x))

  all(sapply(x, is.data.frame))
}

# all_elements_are_named -------------------------------------------------------
all_elements_are_named <- function(x)
{
  stopifnot(is.list(x))

  length(names(x)) == length(x) && all(nzchar(names(x)))
}

# assert_final_slash -----------------------------------------------------------
assert_final_slash <- function(x)
{
  paste0(gsub("/+$", "", x), "/")
}

# clean_stop -------------------------------------------------------------------
clean_stop <- function(...)
{
  stop(..., call. = FALSE)
}

# convert_time_columns ---------------------------------------------------------
convert_time_columns <- function(df, columns = c("createdAt", "updatedAt"))
{
  stopifnot(is.data.frame(df))

  df[columns] <- lapply(df[columns], iso_timestamp_to_local_posix)

  df
}

# create_failure: Create a "failure" result object -----------------------------
create_failure <- function(x)
{
  create_result(success = FALSE, message = as.character(x))
}

# create_result ----------------------------------------------------------------
create_result <- function(
  data = NULL, success = TRUE, message = "Everything ok"
)
{
  list(data = data, sucess = success, message = message)
}

# crop_or_mask -----------------------------------------------------------------
crop_or_mask <- function(x, polygon, use_mask = TRUE)
{
  if (use_mask) {

    raster::mask(x, mask = polygon)

  } else {

    raster::crop(x, y = polygon)
  }
}

# extract_flat_information -----------------------------------------------------
extract_flat_information <- function(x, keep_null = FALSE)
{
  stopifnot(is.list(x))

  is_null <- sapply(x, is.null)

  if (keep_null) {

    # Replace NULL with NA
    x[is_null] <- as.list(rep(NA, sum(is_null)))

  } else {

    x <- kwb.utils::excludeNULL(x, dbg = FALSE)
  }

  # Which elements are (non-list) vectors of length one?
  is_length_one_vector <- ! sapply(x, is.list) & lengths(x) == 1

  # Convert these elements to a data frame with one row
  kwb.utils::asNoFactorDataFrame(x[is_length_one_vector])
}

# flatten_recursive_list -------------------------------------------------------
flatten_recursive_list <- function(x)
{
  stopifnot(is.list(x))

  # Which elements are NULL or of length 0, which are not?
  is_null <- lengths(x) == 0

  # Replace NULL with NA
  x[is_null] <- as.list(rep(NA, sum(is_null)))

  # Which elements are lists, which are not?
  is_list <- vapply(x, is.list, logical(1))

  # All non-list elements are expected to (now) have a length of one
  stopifnot(all(lengths(x[! is_list]) == 1L))

  # Convert the non-list elements to a data frame. If there are no such elements
  # set df_parent to NULL
  df_parent <- if (! all(is_list)) {
    kwb.utils::asNoFactorDataFrame(x[! is_list])
  } # else invisibly NULL

  # Return the parent data if there are no child lists
  if (! any(is_list)) {
    return(df_parent)
  }

  # Flatten all list elements to child data frames
  dfs_children <- lapply(x[is_list], flatten_recursive_list)

  # If all data frames have the same columns, row-bind them, else column-bind
  # them
  #use_rbind <- kwb.utils::allAreIdentical(lapply(dfs_children, names))
  use_rbind <- all(is_list)
  what <- if (use_rbind) dplyr::bind_rows else cbind
  df_children <- do.call(what, dfs_children)

  # Return the child data if there are no parent data
  if (is.null(df_parent)) {
    return(df_children)
  }

  # Column-bind parent data and child data
  df <- cbind(df_parent, df_children, stringsAsFactors = FALSE)

  # Remove any row names
  kwb.utils::resetRowNames(df)
}

# get_environment_var ----------------------------------------------------------
get_environment_var <- function(name)
{
  value <- Sys.getenv(name)

  if (nzchar(value)) {
    return(value)
  }

  clean_stop(sprintf("Please set the environment variable '%s'", name))
}

# get_prefix -------------------------------------------------------------------
get_prefix <- function(x)
{
  parts <- strsplit(x, "_")

  more_than_one_part <- lengths(parts) > 1

  if (! all(more_than_one_part)) {

    stop(
      "The following strings do not have a prefix (separated by underscore):\n",
      kwb.utils::stringList(x[! more_than_one_part]), call. = FALSE
    )
  }

  sapply(parts , "[", 1)
}

# is_error ---------------------------------------------------------------------
is_error <- function(x)
{
  inherits(x, "try-error")
}

# iso_timestamp_to_local_posix -------------------------------------------------
iso_timestamp_to_local_posix <- function(x, tzone = "Europe/Berlin")
{
  stopifnot(is.character(x))

  times <- as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")

  structure(times, tzone = tzone)
}

# new_line_collapsed -----------------------------------------------------------
new_line_collapsed <- function(x)
{
  paste(x, collapse = "\n")
}

# plot_to_svg ------------------------------------------------------------------
plot_to_svg <- function(expr, ..., temp_dir = tempdir())
{
  file <- tempfile("modelplot_", tmpdir = temp_dir, fileext = ".svg")

  grDevices::svg(file, ...)

  on.exit(grDevices::dev.off())

  eval(expr, envir = -1)

  file
}

# reset_time -------------------------------------------------------------------
reset_time <- function(x)
{
  stopifnot(inherits(x, "POSIXct"))

  as.POSIXct(substr(as.character(x), 1, 10), tz = attr(x, "tzone"))
}
