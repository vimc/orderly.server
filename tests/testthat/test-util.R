context("util")

test_that("as_integer", {
  expect_identical(as_integer("1"), 1L)
  expect_identical(as_integer("100"), 100L)
  expect_identical(as_integer(NULL, 500L), 500L)

  x <- 100
  expect_error(as_integer(x), "Invalid input for 'x'")
  expect_error(as_integer(NA_character_), "Invalid input for")
  expect_error(as_integer(c("1", "2")), "Invalid input for")
  expect_error(as_integer("-1"), "Invalid input for")
  expect_error(as_integer("1.23"), "Invalid input for")
})
