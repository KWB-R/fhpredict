# assert_final_slash -----------------------------------------------------------
assert_final_slash <- function(x)
{
  paste0(gsub("/+$", "", x), "/")
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
