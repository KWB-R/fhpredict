skip_on_appveyor()
skip_on_travis()

test_that("api_get_measurements() works", {

  expect_error(
    fhpredict:::api_get_measurements()
    # HTTP request failed: Client error: (404) Not Found
  )

})

