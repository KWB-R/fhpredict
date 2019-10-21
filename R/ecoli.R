# remove_missing_ecoli ---------------------------------------------------------
remove_missing_ecoli <- function(hygiene)
{
  is_missing <- missing_ecoli(hygiene)

  if (! any(is_missing)) {
    return(hygiene)
  }

  kwb.utils::catAndRun(
    sprintf(
      "Removing %d rows where E.coli concentration is missing", sum(is_missing)
    ),
    hygiene[! is_missing, ]
  )
}

# missing_ecoli ----------------------------------------------------------------
missing_ecoli <- function(hygiene)
{
  ecoli <- kwb.utils::selectColumns(hygiene, "e.coli")

  is.na(ecoli) | ecoli == -1
}
