context("api - endpoints")

test_that("index", {
  endpoint <- endpoint_index(NULL)

  expected <- list(name = scalar("orderly.server"),
                    version = scalar("0.0.0"),
                    endpoints = c("comming", "soon"))
  expect_equal(endpoint$target(), expected)

  api <- pkgapi::pkgapi$new()$handle(endpoint)
  res <- api$request("GET", "/")
  expect_equal(res$status, 200L)
  expect_equal(res$headers[["Content-Type"]], "application/json")
  expect_equal(res$body, as.character(endpoint$run()$body))
})


test_that("rebuild", {
  runner <- mock_runner()

  res_target <- target_rebuild(runner)
  expect_null(res_target)

  expect_simple_endpoint_runs(endpoint_rebuild(runner), res_target)
})


test_that("git_status", {
  git_status <- list(success = TRUE, code = 0, output = character(0),
                     clean = TRUE, branch = "master",
                     hash = "bc1afbf30ed83297b4da4dbb8a1930bcab746ac1")
  runner <- mock_runner(git_status = git_status)

  ## First test the basic output:
  res_target <- target_git_status(runner)
  expect_equal(mockery::mock_args(runner$git_status)[[1]], list())
  expect_equal(res_target,
               list(branch = scalar("master"),
                    hash = scalar(git_status$hash),
                    clean = scalar(TRUE),
                    output = character(0)))

  expect_simple_endpoint_runs(endpoint_git_status(runner), res_target)

  expect_equal(mockery::mock_args(runner$git_status)[[1]], list())
  expect_equal(mockery::mock_args(runner$git_status)[[2]], list())
  expect_equal(mockery::mock_args(runner$git_status)[[3]], list())
})


test_that("git_pull", {
  git_pull <- list(
    success = TRUE, code = 0,
    output = c("From upstream",
               "   0fc0d08..0ec7621  master     -> origin/master",
               "Updating 0fc0d08..0ec7621",
               "Fast-forward", " new | 1 +",
               " 1 file changed, 1 insertion(+)",
               " create mode 100644 new"))
  runner <- mock_runner(git_pull = git_pull)

  res <- target_git_pull(runner)
  expect_equal(res, git_pull$output)

  expect_simple_endpoint_runs(endpoint_git_pull(runner), res)

  expect_equal(mockery::mock_args(runner$git_pull)[[1]], list())
  expect_equal(mockery::mock_args(runner$git_pull)[[2]], list())
  expect_equal(mockery::mock_args(runner$git_pull)[[3]], list())
})


test_that("git_fetch", {
  git_fetch <- list(
    success = TRUE, code = 0,
    output = c("From upstream",
               "   0fc0d08..0ec7621  master     -> origin/master"))
  runner <- mock_runner(git_fetch = git_fetch)

  ## First test the basic output:
  res <- target_git_fetch(runner)
  expect_equal(res, git_fetch$output)

  expect_simple_endpoint_runs(endpoint_git_fetch(runner), res)

  expect_equal(mockery::mock_args(runner$git_fetch)[[1]], list())
  expect_equal(mockery::mock_args(runner$git_fetch)[[2]], list())
  expect_equal(mockery::mock_args(runner$git_fetch)[[3]], list())
})


test_that("run", {
  key <- "key-1"
  runner <- mock_runner(keys = key)

  res <- target_run(runner, "example")
  expect_equal(
    res,
    list(name = scalar("example"),
         key = scalar(key),
         path = scalar(sprintf("/v1/reports/%s/status/", key))))
  expect_equal(
    mockery::mock_args(runner$queue)[[1]],
    list("example", NULL, NULL, NULL, TRUE, timeout = 600))

  ## endpoint
  endpoint <- endpoint_run(runner)
  ## TODO: pkgapi bug - running endpoint$run() is 500 not 40x error
  res_endpoint <- endpoint$run("example", timeout = 600)
  expect_equal(res_endpoint$status_code, 200)
  expect_equal(res_endpoint$data, res)

  ## api
  api <- pkgapi::pkgapi$new()$handle(endpoint)
  res_api <- api$request("POST", "/v1/reports/example/run/",
                         list(timeout = 600))
  expect_equal(res_api$status, 200L)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, as.character(res_endpoint$body))
})


test_that("status - queued behind nothing", {
  ## See mock.R
  key <- "key-1"
  status <- list(key = key, status = "queued", id = NA_character_,
                 output = list(stdout = character(), stderr = NULL))

  runner <- mock_runner(key, status)

  res <- target_status(runner, key)
  expect_equal(
    res,
    list(key = scalar(key),
         status = scalar("queued"),
         version = scalar(NA_character_),
         output = list(stdout = character(0), stderr = character(0))))

  expect_equal(mockery::mock_args(runner$status)[[1]], list(key, FALSE))

  ## endpoint
  endpoint <- endpoint_status(runner)
  res_endpoint <- endpoint$run(key)
  expect_equal(res_endpoint$status_code, 200)
  expect_equal(res_endpoint$content_type, "application/json")
  expect_equal(res_endpoint$data, res)
  expect_equal(mockery::mock_args(runner$status)[[2]], list(key, FALSE))

  ## api
  api <- pkgapi::pkgapi$new()$handle(endpoint)
  res_api <- api$request("GET", sprintf("/v1/reports/%s/status/", key))
  expect_equal(res_api$status, 200L)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, as.character(res_endpoint$body))
  expect_equal(mockery::mock_args(runner$status)[[3]], list(key, FALSE))
})


test_that("status - queued", {
  ## See mock.R
  key <- "key-3"
  status <- list(
    key = key, status = "queued", id = NA_character_,
    output = list(stdout = sprintf("queued:key-%d:example", 1:2),
                  stderr = NULL))

  runner <- mock_runner(key, status)

  res <- target_status(runner, key)
  expect_equal(
    res,
    list(key = scalar(key),
         status = scalar("queued"),
         version = scalar(NA_character_),
         output = list(stdout = status$output$stdout, stderr = character(0))))
  expect_equal(mockery::mock_args(runner$status)[[1]], list(key, FALSE))

  ## endpoint
  endpoint <- endpoint_status(runner)
  res_endpoint <- endpoint$run(key)
  expect_equal(res_endpoint$status_code, 200)
  expect_equal(res_endpoint$content_type, "application/json")
  expect_equal(res_endpoint$data, res)
  expect_equal(mockery::mock_args(runner$status)[[2]], list(key, FALSE))

  ## api
  api <- pkgapi::pkgapi$new()$handle(endpoint)
  res_api <- api$request("GET", sprintf("/v1/reports/%s/status/", key))
  expect_equal(res_api$status, 200L)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, as.character(res_endpoint$body))
  expect_equal(mockery::mock_args(runner$status)[[3]], list(key, FALSE))
})


test_that("status - completed, no log", {
  key <- "key-1"
  id <- "20200414-123013-a1df28f7"
  status <- list(key = key, status = "success", id = id, output = NULL)

  runner <- mock_runner(key, status)

  res <- target_status(runner, key)
  expect_equal(
    res,
    list(key = scalar(key),
         status = scalar("success"),
         version = scalar(id),
         output = NULL))
  expect_equal(mockery::mock_args(runner$status)[[1]], list(key, FALSE))

  ## endpoint
  endpoint <- endpoint_status(runner)
  res_endpoint <- endpoint$run(key)
  expect_equal(res_endpoint$status_code, 200)
  expect_equal(res_endpoint$content_type, "application/json")
  expect_equal(res_endpoint$data, res)
  expect_equal(mockery::mock_args(runner$status)[[2]], list(key, FALSE))

  ## api
  api <- pkgapi::pkgapi$new()$handle(endpoint)
  res_api <- api$request("GET", sprintf("/v1/reports/%s/status/", key))
  expect_equal(res_api$status, 200L)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, as.character(res_endpoint$body))
  expect_equal(mockery::mock_args(runner$status)[[3]], list(key, FALSE))
})


test_that("status - completed, with log", {
  key <- "key-1"
  id <- "20200414-123013-a1df28f7"
  status <- list(key = key, status = "success", id = id,
                 output = list(stdout = character(0),
                               stderr = readLines("example/success.txt")))
  runner <- mock_runner(key, status)

  res <- target_status(runner, key, TRUE)
  expect_equal(
    res,
    list(key = scalar(key),
         status = scalar("success"),
         version = scalar(id),
         output = status$output))
  expect_equal(mockery::mock_args(runner$status)[[1]], list(key, TRUE))

  ## endpoint
  endpoint <- endpoint_status(runner)
  res_endpoint <- endpoint$run(key, TRUE)
  expect_equal(res_endpoint$status_code, 200)
  expect_equal(res_endpoint$content_type, "application/json")
  expect_equal(res_endpoint$data, res)
  expect_equal(mockery::mock_args(runner$status)[[2]], list(key, TRUE))

  ## api
  api <- pkgapi::pkgapi$new()$handle(endpoint)
  res_api <- api$request("GET", sprintf("/v1/reports/%s/status/", key),
                         query = list(output = TRUE))
  expect_equal(res_api$status, 200L)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, as.character(res_endpoint$body))
  expect_equal(mockery::mock_args(runner$status)[[3]], list(key, TRUE))
})


test_that("kill - successful", {
  key <- "key-1"
  runner <- mock_runner()

  res <- target_kill(runner, key)
  expect_true(res)
  expect_equal(mockery::mock_args(runner$kill)[[1]], list(key))

  ## endpoint
  endpoint <- endpoint_kill(runner)
  res_endpoint <- endpoint$run(key)
  expect_equal(res_endpoint$status_code, 200)
  expect_equal(res_endpoint$content_type, "application/json")
  expect_equal(res_endpoint$data, res)
  expect_equal(mockery::mock_args(runner$kill)[[2]], list(key))

  ## api
  api <- pkgapi::pkgapi$new()$handle(endpoint)
  res_api <- api$request("DELETE", sprintf("/v1/reports/%s/kill/", key))
  expect_equal(res_api$status, 200L)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, as.character(res_endpoint$body))
  expect_equal(mockery::mock_args(runner$kill)[[3]], list(key))
})


test_that("kill - failure", {
  key <- "key-1"
  runner <- mock_runner()

  msg <- "Can't kill 'key-1' - not currently running a report"
  runner$kill <- mockery::mock(stop(msg), cycle = TRUE)

  res <- expect_error(target_kill(runner, key), class = "pkgapi_error")
  res$trace <- NULL
  expect_equal(mockery::mock_args(runner$kill)[[1]], list(key))
  expect_equal(res$data[[1]]$error, jsonlite::unbox("ERROR"))
  expect_equal(res$data[[1]]$detail, jsonlite::unbox(msg))

  ## endpoint
  endpoint <- endpoint_kill(runner)
  res_endpoint <- endpoint$run(key)
  expect_equal(res_endpoint$status_code, 400)
  expect_equal(res_endpoint$content_type, "application/json")
  expect_null(res_endpoint$data)
  expect_equal(res_endpoint$error, res)
  expect_equal(mockery::mock_args(runner$kill)[[2]], list(key))

  ## api
  api <- pkgapi::pkgapi$new()$handle(endpoint)
  res_api <- api$request("DELETE", sprintf("/v1/reports/%s/kill/", key))
  expect_equal(res_api$status, 400L)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, as.character(res_endpoint$body))
  expect_equal(mockery::mock_args(runner$kill)[[3]], list(key))
})
