context("util")

test_that("is_directory", {
  path <- tempfile()
  expect_false(is_directory(path))
  file.create(path)
  expect_false(is_directory(path))
  expect_true(is_directory(tempdir()))
})


test_that("time_diff_secs", {
  t <- Sys.time()
  expect_equal(time_diff_secs(t, t - 5), 5)
  expect_equal(time_diff_secs(t, t - 500), 500)
  expect_equal(time_diff_secs(t, t - 50000), 50000)
})


test_that("wait_while", {
  expect_null(wait_while(function() FALSE, 0, 0))
  expect_error(wait_while(function() TRUE, 0, 0),
               "Timeout reached")
  expect_error(wait_while(function() TRUE, 0.05, 0.01),
               "Timeout reached")
})
