context("queue")

test_that("queue works as intended", {
  queue <- Queue$new(timeout = 300)
  expect_equal(queue$queue$worker_len(), 1)

  worker_1 <- queue$queue$worker_list()[[1]]

  expect_equal(nrow(queue$queue$worker_log_tail(worker_1)), 1)
  expect_equal(queue$queue$worker_log_tail(worker_1, n = 3)[1, "command"],
               "ALIVE")
  expect_equal(queue$queue$worker_log_tail(worker_1, n = 3)[3, "message"],
               "TIMEOUT_SET")

  expect_length(queue$queue$task_list(), 0)

  ## jobs can be pushed to queue
  job_id <- queue$submit(quote({
    Sys.sleep(1)
    1 + 1
  }))
  expect_length(queue$queue$task_list(), 1)

  ## status can be retireved
  ## sleep to ensure job has been picked up by runner otherwise
  ## will be pending
  Sys.sleep(0.1)
  status <- queue$status(job_id)
  expect_equal(status$status, "running")
  expect_equal(status$queue, 0)

  ## After task has completed
  Sys.sleep(1)
  result <- queue$queue$task_wait(job_id)
  status <- queue$status(job_id)
  expect_equal(status$status, "success")
  expect_equal(status$queue, 0)

  ## Result can be retrieved after task has completed
  res <- queue$result(job_id)
  expect_equal(res, 2)
  expect_length(queue$queue$task_list(), 1)

  ## task can be cleaned up
  queue$remove(job_id)
  expect_length(queue$queue$task_list(), 0)

  ## After task has been added
  job_id <- queue$submit(quote({
    Sys.sleep(10)
    1 + 1
  }))
  expect_length(queue$queue$task_list(), 1)

  ## task can be cancelled
  Sys.sleep(1)
  cancel_output <- queue$cancel(job_id)
  expect_true(cancel_output)
  Sys.sleep(0.5)
  status <- queue$status(job_id)
  expect_equal(status$status, "interrupted")
  expect_length(queue$queue$task_list(), 1)

  ## task can be cleaned up
  queue$remove(job_id)
  expect_length(queue$queue$task_list(), 0)

  con <- queue$queue$con
  key <- queue$queue$keys$worker_name
  expect_equal(con$SCARD(key), 1)

  rm(queue)
  gc()

  expect_equal(con$SCARD(key), 0)
})

test_that("queue_id is generated if not supplied", {
  withr::with_envvar(
    c("ORDERLY_SERVER_QUEUE_ID" = NA),
    expect_match(orderly_queue_id(NULL), "^orderly.server:[[:xdigit:]]+$"))
  withr::with_envvar(
    c("ORDERLY_SERVER_QUEUE_ID" = "myqueue"),
    expect_equal(orderly_queue_id(NULL), "myqueue"))
})


test_that("queue_id is required for workers", {
  withr::with_envvar(
    c("ORDERLY_SERVER_QUEUE_ID" = NA),
    expect_error(orderly_queue_id(NULL, TRUE),
                 "Environment variable 'ORDERLY_SERVER_QUEUE_ID' is not set"))
})

test_that("queue_id is returned if supplied", {
  withr::with_envvar(
    c("ORDERLY_SERVER_QUEUE_ID" = NA),
    expect_equal(orderly_queue_id("myqueue", TRUE), "myqueue"))
})

test_that("test queue can start workers with timeout", {
  skip_if_no_redis()
  queue <- Queue$new(workers = 2, timeout = 300)
  timeout <- queue$queue$message_send_and_wait("TIMEOUT_GET",
                                               queue$queue$worker_list())
  expect_length(timeout, 2)
  expect_equal(timeout[[1]][["timeout"]], 300.0)
  expect_equal(timeout[[2]][["timeout"]], 300.0)
})

test_that("queue starts up normally without a timeout", {
  skip_if_no_redis()
  queue <- Queue$new(workers = 1)
  on.exit(queue$cleanup())
  timeout <- queue$queue$message_send_and_wait("TIMEOUT_GET",
                                               queue$queue$worker_list(),
                                               progress = FALSE)
  expect_equal(timeout[[1]], c("timeout" = Inf, remaining = Inf))
})
