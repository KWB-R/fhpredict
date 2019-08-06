# api_get_bathingspot ----------------------------------------------------------

#' Get Bathing Spot(s)
#'
#' @param user_id user ID or -1L (all users)
#' @param spot_id bathing spot ID or -1L (all bathing spots)
#' @param pattern optional. Pattern matching the names of bathing spots to be
#'   returned
#' @export
api_get_bathingspot <- function(user_id = -1L, spot_id = -1L, pattern = "")
{
  #kwb.utils::assignPackageObjects("fhpredict")

  # If a user_id is given, check if the user exists
  check_user_id(user_id)

  spots <- run_cached(
    name = path_bathingspot(user_id, spot_id, sep = "-"),
    expr = {
      content <- safe_postgres_get(path_bathingspot(user_id, spot_id))
      spots <- do.call(rbind, lapply(content$data, function(x) {
        kwb.utils::asNoFactorDataFrame(
          kwb.utils::selectElements(x, c("id", "name"))
        )
      }))
    }
  )

  if (pattern == "") {
    return(spots)
  }

  spots[grepl(pattern, spots$name, ignore.case = TRUE), ]
}
