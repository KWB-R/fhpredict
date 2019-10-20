# remove_and_reorder_columns ---------------------------------------------------
remove_and_reorder_columns <- function(df, type = NULL)
{
  columns_modtime <- c("createdAt", "updatedAt")

  # Define columns to remove and columns to appear first
  column_lists <- list(
    bathingspots = list(
      remove = c("nameLong", "postalCode", columns_modtime),
      first = c(
        "id", "name", "water", "city", "district"
      )
    ),
    irradiances = list(
      remove = c(columns_modtime, "date", "comment"),
      first = c("id", "dateTime", "value")
    ),
    predictions = list(
      remove = c(columns_modtime, "oldId", "date"),
      first = c("id", "dateTime", "prediction")
    ),
    rain = list(
      remove = columns_modtime,
      first = c("id")
    ),
    users = list(
      remove = c("auth0Id", "version", columns_modtime),
      first = c("id", "firstName", "lastName", "role")
    )
  )

  typelist <- kwb.utils::stringList(names(column_lists))

  if (is.null(type)) {
    clean_stop(get_text("specify_type", typelist = typelist))
  }

  info <- column_lists[[type]]

  if (is.null(info)) {
    clean_stop(get_text("no_specification", type = type, typelist = typelist))
  }

  # Extract column vectors
  columns_remove <- kwb.utils::selectElements(info, "remove")
  columns_first <- kwb.utils::selectElements(info, "first")

  # Return the data frame if it does not have any columns
  if (ncol(df) == 0) {
    return(df)
  }

  # Remove columns
  df <- kwb.utils::removeColumns(df, columns_remove)

  # Move columns to the front
  kwb.utils::moveColumnsToFront(df, columns_first)
}
