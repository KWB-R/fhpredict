# api_measurements_spot --------------------------------------------------------
api_measurements_spot <- function(user_id = -1, spot_id = -1)
{
  #run_cached(name = "measurements_spot", {

    path <- path_measurements(user_id, spot_id)

    result <- safe_postgres_get(path)

    measurements <- kwb.utils::safeRowBindAll(lapply(result$data, function(x) {
      kwb.utils::asNoFactorDataFrame(kwb.utils::excludeNULL(x, dbg = FALSE))
    }))

    kwb.utils::removeColumns(measurements, pattern = "At$")
  #})
}
