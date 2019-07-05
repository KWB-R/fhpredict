# assert_final_slash -----------------------------------------------------------
assert_final_slash <- function(x)
{
  paste0(gsub("/+$", "", x), "/")
}
