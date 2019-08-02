# assert_final_slash -----------------------------------------------------------
assert_final_slash <- function(x)
{
  paste0(gsub("/+$", "", x), "/")
}

# flatten_recursive_list -------------------------------------------------------
flatten_recursive_list <- function(x)
{
  stopifnot(is.list(x))

  if (length(x) == 0) {
    return(data.frame())
  }

  x <- kwb.utils::excludeNULL(x, dbg = FALSE)

  x <- x[! sapply(x, is.list)]

  stopifnot(all(lengths(x) == 1))

  kwb.utils::asNoFactorDataFrame(x)
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

# clean_stop -------------------------------------------------------------------
clean_stop <- function(...)
{
  stop(..., call. = FALSE)
}
