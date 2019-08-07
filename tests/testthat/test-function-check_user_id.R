skip_on_appveyor()
skip_on_travis()

test_that("check_user_id() works", {

  expect_error(
    fhpredict:::check_user_id()
    # argument "user_id" is missing, with no default
  )

})

