skip_on_appveyor()
skip_on_travis()

test_that("api_get_users() works", {

  fhpredict:::api_get_users()

})
