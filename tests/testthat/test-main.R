context("main")

test_that("defaults", {
  expect_equal(
    main_args("path"),
    list(path = "path", port = 8321, host = "0.0.0.0", allow_ref = TRUE,
         go_signal = NULL, queue_id = NULL, workers = 0, backup_period = 600,
         log_level = "info"))
})


test_that("set port", {
  expect_equal(
    main_args(c("path", "--port", "8888")),
    list(path = "path", port = 8888, host = "0.0.0.0", allow_ref = TRUE,
         go_signal = NULL, queue_id = NULL, workers = 0, backup_period = 600,
         log_level = "info"))
})


test_that("set host", {
  expect_equal(
    main_args(c("path", "--host", "127.0.0.1")),
    list(path = "path", port = 8321, host = "127.0.0.1", allow_ref = TRUE,
         go_signal = NULL, queue_id = NULL, workers = 0, backup_period = 600,
         log_level = "info"))
})


test_that("prevent reference switch", {
  expect_equal(
    main_args(c("path", "--no-ref")),
    list(path = "path", port = 8321, host = "0.0.0.0", allow_ref = FALSE,
         go_signal = NULL, queue_id = NULL, workers = 0, backup_period = 600,
         log_level = "info"))
})


test_that("Set go signal", {
  expect_equal(
    main_args(c("path", "--go-signal", "somewhere")),
    list(path = "path", port = 8321, host = "0.0.0.0", allow_ref = TRUE,
         go_signal = "somewhere", queue_id = NULL, workers = 0,
         backup_period = 600, log_level = "info"))
})


test_that("Set workers", {
  expect_equal(
    main_args(c("path", "--workers", "2")),
    list(path = "path", port = 8321, host = "0.0.0.0", allow_ref = TRUE,
         go_signal = NULL, queue_id = NULL, workers = 2, backup_period = 600,
         log_level = "info"))
})


test_that("Set queue id", {
  expect_equal(
    main_args(c("path", "--queue-id", "orderly")),
    list(path = "path", port = 8321, host = "0.0.0.0", allow_ref = TRUE,
         go_signal = NULL, queue_id = "orderly", workers = 0,
         backup_period = 600, log_level = "info"))
})

test_that("Set backup period", {
  expect_equal(
    main_args(c("path", "--backup-period", "0")),
    list(path = "path", port = 8321, host = "0.0.0.0", allow_ref = TRUE,
         go_signal = NULL, queue_id = NULL, workers = 0, backup_period = NULL,
         log_level = "info"))
})

test_that("Set log level", {
  expect_equal(
    main_args(c("path", "--log-level", "trace")),
    list(path = "path", port = 8321, host = "0.0.0.0", allow_ref = TRUE,
         go_signal = NULL, queue_id = NULL, workers = 0, backup_period = 600,
         log_level = "trace"))
})


test_that("write script", {
  p <- tempfile()
  expect_error(write_script(p), "'path' must be a directory")
  dir.create(p)
  res <- write_script(p, "orderly.server:::main()")
  expect_equal(basename(res), "orderly.server")
  expect_true(file.exists(res))
})


test_that("write script produces sensible script", {
  path <- tempfile()
  dir.create(path, FALSE, TRUE)
  bin <- write_script(
    path,
    readLines(system.file("script", package = "orderly", mustWork = TRUE)))
  expect_equal(basename(bin), "orderly.server")
  expect_true(file.exists(bin))
  expect_equal(readLines(bin)[[1]], "#!/usr/bin/env Rscript")
})


test_that("write script can be versioned", {
  path <- tempfile()
  dir.create(path, FALSE, TRUE)
  bin <- write_script(
    path,
    readLines(system.file("script", package = "orderly", mustWork = TRUE)),
    TRUE)
  expect_match(readLines(bin)[[1]], R.home(), fixed = TRUE)
})


test_that("pass arguments to server", {
  mock_server <- mockery::mock(NULL)
  with_mock("orderly.server:::server" = mock_server,
            main("path"))
  expect_equal(
    mockery::mock_args(mock_server)[[1]],
    main_args("path"))
})


test_that("main_worker_args", {
  expect_equal(main_worker_args(c()),
               list(queue_id = NULL,
                    go_signal = NULL))
  expect_equal(main_worker_args("orderly"),
               list(queue_id = "orderly",
                    go_signal = NULL))
  expect_equal(main_worker_args(c("--go-signal", "/go-signal", "orderly")),
               list(queue_id = "orderly",
                    go_signal = "/go-signal"))
})


test_that("main worker starts rrq worker", {
  mock_rrq_worker <- mockery::mock(list(loop = function() TRUE), cycle = TRUE)
  with_mock("rrq::rrq_worker_from_config" = mock_rrq_worker, {
    worker <- main_worker("queue_id")
  })
  args <- mockery::mock_args(mock_rrq_worker)[[1]]
  expect_equal(args[[1]], "queue_id")
})

test_that("main worker waits for go signal", {
  path <- tempfile()
  dir.create(path)

  go_signal <- file.path(path, "go")
  dt <- as.difftime(1, units = "secs")
  mock_rrq_worker <- mockery::mock(list(loop = function() TRUE), cycle = TRUE)
  mock_wait_while <- mockery::mock(dt, cycle = TRUE)
  msg <- capture_messages(
    with_mock("orderly.server:::wait_while" = mock_wait_while,
              "rrq::rrq_worker_from_config" = mock_rrq_worker,
              worker <- main_worker(c("--go-signal", go_signal, "queue_id"))))

  mockery::expect_called(mock_wait_while, 1)
  args <- mockery::mock_args(mock_rrq_worker)[[1]]
  expect_equal(args[[1]], "queue_id")
})
