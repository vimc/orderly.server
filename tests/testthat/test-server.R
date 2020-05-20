context("server")

test_that("Don't wait if the go signal path is NULL", {
  path <- tempfile()
  dt <- as.difftime(1, units = "secs")
  mock_wait_while <- mockery::mock(dt, cycle = TRUE)
  expect_silent(
    with_mock("wait_while" = mock_wait_while,
              wait_for_go_signal(path, NULL)))
  mockery::expect_called(mock_wait_while, 0)
})


test_that("Wait for a go signal if provided", {
  path <- tempfile()
  dir.create(path)

  go <- "go"
  dt <- as.difftime(1, units = "secs")
  mock_wait_while <- mockery::mock(dt, cycle = TRUE)
  msg <- capture_messages(
    with_mock("wait_while" = mock_wait_while,
              wait_for_go_signal(path, go)))
  mockery::expect_called(mock_wait_while, 1)
  args <- mockery::mock_args(mock_wait_while)[[1]]
  expect_is(args[[1]], "function")
  expect_equal(args[[2]], 600)
  expect_equal(args[[3]], 1)

  expect_true(args[[1]]())
  file.create(file.path(path, go))
  expect_false(args[[1]]())

  expect_match(msg[[1]], "Waiting for go signal at")
  expect_match(msg[[2]], "Recieved go signal after 1")
})
