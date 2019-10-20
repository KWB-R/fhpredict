test_that("remove_and_reorder()", {

  f <- fhpredict:::remove_and_reorder_columns

  users <- data.frame(
    lastName = "last", firstName = "first", id = 1, auth0Id = "xyz",
    role = "boss"
  )

  expect_error(f(), "specify 'type'")
  expect_error(f(type = "abc"), "No specification found")

  expect_identical(
    names(f(users, "users")), c("id", "firstName", "lastName", "role")
  )
})
