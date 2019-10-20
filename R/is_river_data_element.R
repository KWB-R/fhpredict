# is_river_data_element --------------------------------------------------------
is_river_data_element <- function(x)
{
  # All elements are data frames
  if (! all_elements_are_data_frames(x)) {

    message("Not all elements are data frames")
    return(FALSE)
  }

  # All elements are named
  if (! all_elements_are_named(x)) {
    message("Not all elements are named")
    return(FALSE)
  }

  # All element names must be prefixed
  prefixes <- get_prefix(names(x))

  # All prefixes must be unique
  if (any(duplicated(prefixes))) {
    message("There are duplicated prefixes: ", prefixes[duplicated(prefixes)])
    return(FALSE)
  }

  # There must be a prefix "hygiene"
  if (! "hygiene" %in% prefixes) {
    message("There is no prefix 'hygiene'")
    return(FALSE)
  }

  # All prefixes must be one of "hygiene", "i", "ka", "q", "r"
  unexpected <- prefixes[! prefixes %in% expected_prefixes()]

  if (length(unexpected)) {
    message(
      "There are unexpected prefixes: ", kwb.utils::stringList(unexpected),
      ". Expected prefixes: ", kwb.utils::stringList(expected_prefixes())
    )
    return(FALSE)
  }

  # All tables must be river data tables
  for (i in seq_along(x)) {

    if (! is_river_data_table(x[[i]], prefixes[i])) {

      return(FALSE)
    }
  }

  TRUE
}

# expected_prefixes ------------------------------------------------------------
expected_prefixes <- function()
{
  c("hygiene", "i", "ka", "q", "r")
}

# is_river_data_table ----------------------------------------------------------
is_river_data_table <- function(x, prefix = "no_prefix_given")
{
  if (! length(prefix) == 1 && prefix %in% expected_prefixes()) {
    stop(
      "prefix ('", prefix, "') must be one of ",
      kwb.utils::stringList(expected_prefixes()), call. = FALSE
    )
  }

  if (! is.data.frame(x)) {
    message("x is not a data frame")
    return(FALSE)
  }

  if (! all_elements_are_named(x)) {
    message("not all columns are named")
    return(FALSE)
  }

  if (! names(x)[1] == "datum") {
    message(
      "The first column name is not 'datum' but: ",
      kwb.utils::stringList(names(x)[1])
    )
    return(FALSE)
  }

  if (prefix == "hygiene") {

    if (! identical(names(x)[1:2], c("datum", "e.coli"))) {
      message(
        "The first two column names of the hygiene table are not 'datum', ",
        "'e.coli' but:", kwb.utils::stringList(names(x)[1:2])
      )
      return(FALSE)
    }

  } else {

    prefixes <- get_prefix(names(x)[-1])

    expected <- c(prefix, if (prefix == "i") "sd")

    if (! all(prefixes %in% expected)) {

      message(
        "Not all columns are prefixed with one of ",
        kwb.utils::stringList(expected), ": ",
        kwb.utils::stringList(prefixes)
      )
      return(FALSE)
    }
  }

  TRUE
}
