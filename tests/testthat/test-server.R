context("server")

test_that("Don't wait if the go signal path is NULL", {
  path <- tempfile()
  dt <- as.difftime(1, units = "secs")
  mock_wait_while <- mockery::mock(dt, cycle = TRUE)
  expect_silent(
    with_mock("orderly.server:::wait_while" = mock_wait_while,
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
    with_mock("orderly.server:::wait_while" = mock_wait_while,
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


test_that("run server", {
  path <- tempfile()
  port <- 1234
  host <- "127.0.0.1"
  allow_ref <- FALSE
  go_signal <- "go"

  api <- list(run = mockery::mock())
  runner <- mock_runner(root = path)

  mock_wait_for_go_signal <- mockery::mock()
  mock_orderly_runner <- mockery::mock(runner)
  mock_build_api <- mockery::mock(api)


  msg <- capture_messages(
    with_mock(
      "orderly.server:::wait_for_go_signal" = mock_wait_for_go_signal,
      "orderly.server:::orderly_runner" = mock_orderly_runner,
      "orderly.server:::build_api" = mock_build_api,
      server(path, port, host, allow_ref, go_signal)))
  expect_match(msg[[1]], "Starting orderly server on port 1234")
  expect_match(msg[[2]], "Orderly root:")
  expect_match(msg[[3]], "Server exiting")

  mockery::expect_called(mock_wait_for_go_signal, 1)
  expect_equal(
    mockery::mock_args(mock_wait_for_go_signal)[[1]],
    list(path, go_signal))

  mockery::expect_called(mock_orderly_runner, 1)
  expect_equal(
    mockery::mock_args(mock_orderly_runner)[[1]],
    list(path, allow_ref, queue_id = NULL, workers = 1))

  mockery::expect_called(mock_build_api, 1)
  args_build <- mockery::mock_args(mock_build_api)[[1]]
  expect_length(args_build, 5)
  expect_identical(args_build[[1]], runner)
  expect_equal(args_build[[2]], path)
  expect_equal(args_build$backup_period, 600)
  expect_equal(args_build$rate_limit, 120)
  expect_s3_class(args_build$logger, "Logger")

  mockery::expect_called(api$run, 1)
  expect_equal(
    mockery::mock_args(api$run)[[1]],
    list(host, port))
})
