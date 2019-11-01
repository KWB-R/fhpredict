test_that("remove_empty_data_frames() works", {

  f <- fhpredict:::remove_empty_data_frames

  expect_error(f())
  expect_error(f(1))
  expect_identical(f(list()), list())
  expect_error(f(list(1)))
  expect_error(f(list(data.frame(), 1)))
  expect_identical(f(list(data.frame(), data.frame())), list())

  b <- data.frame(x = 1)
  input <- list(a = data.frame(), b = b)
  expect_identical(f(input), list(b = b))
})
