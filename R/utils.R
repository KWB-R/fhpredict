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

# apply_at_index ---------------------------------------------------------------

#' Apply a Function to List Elements at Given Index
#'
#' @param x a list or a data frame (which is in fact a list)
#' @param i vector of indices of elements in \code{x} to which the function
#'   \code{fun} is to be applied
#' @param fun function to be applied
#' @param \dots further arguments passed to \code{fun}
#' @param prefix optional. String used to prefix the names of the elements to
#'   which the function was applied. By default the name of the function
#'   \code{fun} is used as a prefix
#' @examples
#' (x <- list(a = 1, b = 2, c = 3))
#' fhpredict:::apply_at_index(x, i = -1, fun = exp)
#'
#' (x <- data.frame(a = -(1:2), b = -(2:3), c = c("a", "b")))
#' fhpredict:::apply_at_index(x, i = -3, fun = abs)
#'
apply_at_index <- function(x, i, fun, ..., prefix = NULL)
{
  stopifnot(is.list(x))

  if (is.null(prefix)) {
    prefix <- paste0(deparse(substitute(fun)), "_")
  }

  x[i] <- lapply(x[i], fun, ...)

  names(x)[i] <- paste0(prefix, names(x)[i])

  x
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
  list(data = data, success = success, message = message)
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

# empty_ggplot -----------------------------------------------------------------
empty_ggplot <- function(text = "Nothing to plot.")
{
  data <- data.frame(x = 0, y = 0, label = text)

  ggplot2::ggplot(data, ggplot2::aes(.data$x, .data$y)) +
    ggplot2::geom_text(ggplot2::aes(label = .data$label)) +
    ggplot2::theme_void()
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

# identify_date_duplicates -----------------------------------------------------
identify_date_duplicates <- function(x, date_column = "datum")
{
  stopifnot(is.list(x))

  if (! is.data.frame(x)) {
    return(lapply(x, identify_date_duplicates, date_column = date_column))
  }

  stopifnot(is.data.frame(x))

  counts <- table(kwb.utils::selectColumns(x, date_column))
  counts[counts > 1L]
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

# get_independent_variables ----------------------------------------------------
get_independent_variables <- function(x)
{
  stopifnot(rlang::is_formula(x))

  all.vars(x)[-1L]
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

# merge_data_frames ------------------------------------------------------------
merge_data_frames <- function(x, by, all = FALSE)
{
  stopifnot(is.list(x))
  stopifnot(length(x) > 0)
  stopifnot(all(sapply(x, is.data.frame)))

  result <- x[[1]]

  for (df in x[-1]) {
    result <- merge(result, df, by = "datum", all = all)
  }

  result
}

# new_line_collapsed -----------------------------------------------------------
new_line_collapsed <- function(x)
{
  paste(x, collapse = "\n")
}

# plot_to_svg ------------------------------------------------------------------
plot_to_svg <- function(expr, width = 8, height = 6, ..., temp_dir = tempdir())
{
  file <- tempfile("modelplot_", tmpdir = temp_dir, fileext = ".svg")

  if (inherits(expr, "ggplot")) {

    ggplot2::ggsave(file, plot = expr, width = width, height = height, ...)

  } else {

    grDevices::svg(file, width = width, height = height, ...)
    on.exit(grDevices::dev.off())
    eval(expr, envir = -1)
  }

  file
}

# remove_empty_data_frames -----------------------------------------------------
remove_empty_data_frames <- function(x)
{
  stopifnot(is.list(x))
  stopifnot(all(sapply(x, is.data.frame)))

  has_no_rows <- sapply(x, nrow) == 0

  if (! any(has_no_rows)) {
    return(x)
  }

  kwb.utils::catAndRun(
    sprintf(
      "Removing %d empty data frames from '%s': %s",
      sum(has_no_rows), deparse(substitute(x)),
      kwb.utils::stringList(names(x)[has_no_rows])
    ),
    expr = x[! has_no_rows]
  )
}

# reset_time -------------------------------------------------------------------
reset_time <- function(x)
{
  stopifnot(inherits(x, "POSIXct"))

  as.POSIXct(substr(as.character(x), 1, 10), tz = attr(x, "tzone"))
}

# safe_log10 -------------------------------------------------------------------
safe_log10 <- function(x, offset = 1)
{
  log10(x + offset)
}

# structure_as_text ------------------------------------------------------------
structure_as_text <- function(x)
{
  paste(collapse = "\n", utils::capture.output(utils::str(x)))
}
