context("util (assert)")

test_that("assert_scalar", {
  expect_error(assert_scalar(NULL), "must be a scalar")
  expect_error(assert_scalar(numeric(0)), "must be a scalar")
  expect_error(assert_scalar(1:2), "must be a scalar")
})

test_that("assert_character", {
  expect_error(assert_character(1), "must be character")
  expect_error(assert_character(TRUE), "must be character")
})

test_that("assert_numeric", {
  expect_error(assert_numeric("one"), "must be numeric")
  expect_error(assert_numeric(TRUE), "must be numeric")
})
