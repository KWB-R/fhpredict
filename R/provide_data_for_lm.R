# provide_data_for_lm ----------------------------------------------------------
provide_data_for_lm <- function(
  riverdata, pattern = "", dbg = TRUE, for_model_building = TRUE
)
{
  #pattern="";dbg=TRUE
  `%>%` <- magrittr::`%>%`

  unrolled_data <- riverdata %>%
    remove_hygiene_data() %>%
    kwb.flusshygiene::unroll_physical_data()

  # Prepare variables out of all combinations (given by pattern)
  # Variables for interaction get replaced by q_new (remove q_old)
  all_columns <- lapply(unrolled_data, names)

  # Determine variable names: all different column names except "datum"
  variables <- setdiff(unique(unlist(all_columns)), "datum")

  # Filter for variables matching the pattern
  if (nzchar(pattern)) {

    variables <- kwb.utils::catAndRun(
      get_text("filtering_variables", n = length(variables), pattern = pattern),
      expr = grep(pattern, variables, value = TRUE),
      dbg = dbg
    )
  }

  kwb.utils::catIf(dbg, get_text(
    "using_variables",
    n = length(variables),
    varlist = kwb.utils::stringList(variables, collapse = "\n- ")
  ))

  variables <- c("log_e.coli", variables)

  # Prepare formulas
  data <- if (for_model_building) {
    kwb.flusshygiene::process_model_riverdata(riverdata, variables)
  } else {
    process_model_data(riverdata, variables)
  }

  if (nrow(data) == 0) {
    utils::str(riverdata)
    clean_stop(get_text("process_returned_no_data"))
  }

  data
}

# process_model_data -----------------------------------------------------------
# modified version of kwb.flusshygiene::process_model_riverdata
process_model_data <- function (riverdata, variables)
{
  hygiene_df <- riverdata[[grep("hygiene", names(riverdata))]]
  typedata <- riverdata[! grepl("hygiene", names(riverdata))]

  variables <- variables[!grepl(":", variables)]

  log <- grepl("^log", variables[1])

  prefix <- sub("^(log_)?([a-z]{1,3})_.*", "\\2", variables[-1])

  log_prefix <- unique(prefix[grepl("^log", variables[-1])])

  data_prefix <- sub("^([a-z]{1,3})_.*", "\\1", names(typedata))

  log <- c(log, data_prefix %in% log_prefix)

  names(log) <- c("e.coli", names(typedata))

  model_list <- if (log[["e.coli"]]) {

    hygiene_df$log_e.coli <- log10(hygiene_df$e.coli)

    list(hygiene = subset(hygiene_df, select = c("datum", "log_e.coli")))

  } else {

    list(hygiene = subset(hygiene_df, select = c("datum", "e.coli")))
  }

  logify <- function(df) {

    df[, -1] <- lapply(df[, -1], function(x) {
      log10(x + 1)
    })

    names(df)[-1] <- paste0("log_", names(df)[-1])

    return(df)
  }

  to_be_logified <- log[-1]
  typedata[to_be_logified] <- lapply(typedata[to_be_logified], logify)

  unrolled_typedata <- kwb.flusshygiene::unroll_physical_data(typedata)

  for (df in unrolled_typedata) {

    if (any(select_x <- names(df) %in% variables)) {

      model_list[[max(names(df)[select_x])]] <- df[, c(TRUE, select_x[-1])]
    }
  }

  model_df <- model_list[[1]]

  for (df in model_list[-1]) {

    model_df <- merge(model_df, df, by = "datum", all = TRUE)
  }

  model_df
}

# remove_hygiene_data ----------------------------------------------------------
remove_hygiene_data <- function(datalist)
{
  stopifnot(is_river_data_element(datalist))

  hygiene_element <- grep("^hygiene", names(datalist), value = TRUE)

  result <- kwb.utils::catAndRun(
    get_text("removing_data_frame", element = hygiene_element),
    datalist[setdiff(names(datalist), hygiene_element)]
  )

  result
}
