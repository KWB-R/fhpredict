# body_measurement -------------------------------------------------------------
body_measurement <- function(date, dateTime, value, comment = "any comment?")
{
  list(
    date = date,
    dateTime = dateTime,
    value = value,
    comment = comment
  )
}

# body_model -------------------------------------------------------------------
body_model <- function(
  rmodel, comment = "any comment?", parameter = "conc_ec or conc_ie?"
)
{
  list(
    rmodel = rmodel,
    comment = comment,
    parameter = parameter
  )
}
