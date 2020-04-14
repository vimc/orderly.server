context("api - endpoints")

## These would all be heaps easier with a mock runner

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

  ## First test the basic output:
  res_target <- target_rebuild(runner)
  expect_null(res_target)

  ## endpoint
  endpoint <- endpoint_rebuild(runner)
  res_endpoint <- endpoint$run()
  expect_equal(res_endpoint$status_code, 200)
  expect_null(res_endpoint$data)

  ## api
  api <- pkgapi::pkgapi$new()$handle(endpoint)
  res_api <- api$request("POST", "/v1/reports/rebuild/")
  expect_equal(res_api$status, 200L)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, as.character(res_endpoint$body))
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

  ## endpoint
  endpoint <- endpoint_git_status(runner)
  res_endpoint <- endpoint$run()
  expect_equal(res_endpoint$status_code, 200)
  expect_equal(res_endpoint$data, res_target)
  expect_equal(mockery::mock_args(runner$git_status)[[1]], list())

  ## api
  api <- pkgapi::pkgapi$new()$handle(endpoint)
  res_api <- api$request("GET", "/v1/reports/git/status/")
  expect_equal(res_api$status, 200L)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, as.character(res_endpoint$body))
  expect_equal(mockery::mock_args(runner$git_status)[[1]], list())
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

  ## endpoint
  endpoint <- endpoint_git_pull(runner)
  res_endpoint <- endpoint$run()
  expect_equal(res_endpoint$status_code, 200)
  expect_equal(res_endpoint$data, res)

  ## api
  api <- pkgapi::pkgapi$new()$handle(endpoint)
  res_api <- api$request("POST", "/v1/reports/git/pull/")
  expect_equal(res_api$status, 200L)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, as.character(res_endpoint$body))
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

  ## endpoint
  endpoint <- endpoint_git_fetch(runner)
  res_endpoint <- endpoint$run()
  expect_equal(res_endpoint$status_code, 200)
  expect_is(res_endpoint$data, "character")

  ## api
  api <- pkgapi::pkgapi$new()$handle(endpoint)
  res_api <- api$request("POST", "/v1/reports/git/fetch/")
  expect_equal(res_api$status, 200L)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, as.character(res_endpoint$body))
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
    list("example", NULL, NULL, FALSE, timeout = 600))

  ## endpoint
  endpoint <- endpoint_run(runner)
  ## TODO: pkgapi bug - running endpoint$run() is 500 not 40x error
  res_endpoint <- endpoint$run("example")
  expect_equal(res_endpoint$status_code, 200)
  expect_equal(res_endpoint$data, res)

  ## api
  api <- pkgapi::pkgapi$new()$handle(endpoint)
  res_api <- api$request("POST", "/v1/reports/example/run/")
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
