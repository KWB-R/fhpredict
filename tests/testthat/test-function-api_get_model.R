skip_on_appveyor()
skip_on_travis()

test_that("api_get_model() works", {

  expect_error(
    fhpredict:::api_get_model()
    # argument "user_id" is missing, with no default
  )

})

