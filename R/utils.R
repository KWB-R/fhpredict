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

  x <- kwb.utils::excludeNULL(x)

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

  stop(sprintf("Please set the environment variable '%s'", name), call. = FALSE)
}
