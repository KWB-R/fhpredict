test_that("flatten_recursive_list() works", {
  #library(testthat)

  f <- fhpredict:::flatten_recursive_list

  # Error: must be a list
  expect_error(f(NULL))
  expect_error(f(numeric()))
  expect_error(f(character()))
  expect_error(f(1))
  expect_error(f("a"))
  expect_error(f(1:4))

  expect_null(f(list()))

  expect_identical(
    f(list(x = 1, y = "b")),
    data.frame(x = 1, y = "b", stringsAsFactors = FALSE)
  )

  expect_identical(
    f(list(x = "a", y = NULL, z = TRUE)),
    data.frame(x = "a", y = NA, z = TRUE, stringsAsFactors = FALSE)
  )

  expect_identical(
    f(x = list(list(x = "a1", y = "b1"), list(x = "a2", y = "b2"))),
    data.frame(x = c("a1", "a2"), y = c("b1", "b2"), stringsAsFactors = FALSE)
  )

  expect_identical(
    f(x = list(a = 1, b = 2, c = list(
      list(x = "a1", y = "b1"), list(x = "a2", y = "b2")
    ))),
    data.frame(
      a = 1, b = 2, c.x = c("a1", "a2"), c.y = c("b1", "b2"),
      stringsAsFactors = FALSE
    )
  )

  x <- list(
    id = 1,
    name = "peter",
    addr = list(
      street = "Cicerostr.",
      no = 24,
      zip = 10709,
      city = "Berlin"
    ),
    sisters = list(
      list(
        first = "mary",
        last = "meyer"
      ),
      list(
        first = "anna",
        last = "smith"
      )
    )
  )

  xf <- f(x)

  expect_identical(nrow(xf), 2L)
  expect_identical(names(xf), c(
    "id", "name", "addr.street", "addr.no", "addr.zip", "addr.city",
    "sisters.first", "sisters.last"
  ))
})
