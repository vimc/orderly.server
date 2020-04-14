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
  runner <- test_runner2()

  ## First test the basic output:
  res_target <- testthat::evaluate_promise(
    target_rebuild(runner))
  expect_null(res_target$result, NA)
  expect_equal(res_target$messages, "[ rebuild    ]  db\n")

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
  path <- orderly:::prepare_orderly_git_example()
  runner <- orderly::orderly_runner(path[["local"]])

  cmp <- runner$git_status()

  ## First test the basic output:
  res_target <- target_git_status(runner)
  expect_equal(res_target$branch, scalar("master"))
  expect_equal(res_target$hash, scalar(cmp$hash))
  expect_equal(res_target$clean, scalar(TRUE))
  expect_equal(res_target$output, character(0))

  ## endpoint
  endpoint <- endpoint_git_status(runner)
  res_endpoint <- endpoint$run()
  expect_equal(res_endpoint$status_code, 200)
  expect_equal(res_endpoint$data, res_target)

  ## api
  api <- pkgapi::pkgapi$new()$handle(endpoint)
  res_api <- api$request("GET", "/v1/reports/git/status/")
  expect_equal(res_api$status, 200L)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, as.character(res_endpoint$body))
})


test_that("git_pull", {
  path <- orderly:::prepare_orderly_git_example()
  runner <- orderly::orderly_runner(path[["local"]])

  ## First test the basic output:
  res <- testthat::evaluate_promise(
    target_git_pull(runner))
  res_target <- res$result
  cmp <- sub(".*\\]  ", "", strsplit(res$messages[[2]], "\n")[[1]])
  expect_equal(res_target, cmp)

  ## endpoint
  endpoint <- endpoint_git_pull(runner)
  res_endpoint <- endpoint$run()
  expect_equal(res_endpoint$status_code, 200)
  expect_is(res_endpoint$data, "character")

  ## api
  api <- pkgapi::pkgapi$new()$handle(endpoint)
  res_api <- api$request("POST", "/v1/reports/git/pull/")
  expect_equal(res_api$status, 200L)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, as.character(res_endpoint$body))
})


test_that("git_fetch", {
  path <- orderly:::prepare_orderly_git_example()
  runner <- orderly::orderly_runner(path[["local"]])

  ## First test the basic output:
  res <- testthat::evaluate_promise(
    target_git_fetch(runner))
  res_target <- res$result
  cmp <- sub(".*\\]  ", "", strsplit(res$messages[[2]], "\n")[[1]])
  expect_equal(res_target, cmp)

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


test_that("status - queued", {
  runner <- test_runner2()
  key <- runner$queue("example")

  res1 <- target_status(runner, key)
  res2 <- target_status(runner, key, TRUE)
  expect_equal(
    res1,
    list(key = scalar(key),
         status = scalar("queued"),
         version = scalar(NA_character_),
         output = NA))
  expect_equal(
    res2,
    list(key = scalar(key),
         status = scalar("queued"),
         version = scalar(NA_character_),
         output = list(stdout = character(0), stderr = character(0))))

  ## endpoint
  endpoint <- endpoint_status(runner)
  res1_endpoint <- endpoint$run(key)
  expect_equal(res1_endpoint$status_code, 200)
  expect_equal(res1_endpoint$content_type, "application/json")
  expect_equal(res1_endpoint$data, res1)

  res2_endpoint <- endpoint$run(key, TRUE)
  expect_equal(res2_endpoint$status_code, 200)
  expect_equal(res2_endpoint$content_type, "application/json")
  expect_equal(res2_endpoint$data, res2)

  ## api
  api <- pkgapi::pkgapi$new()$handle(endpoint)
  res1_api <- api$request("GET", sprintf("/v1/reports/%s/status/", key))
  expect_equal(res1_api$status, 200L)
  expect_equal(res1_api$headers[["Content-Type"]], "application/json")
  expect_equal(res1_api$body, as.character(res1_endpoint$body))

  res2_api <- api$request("GET", sprintf("/v1/reports/%s/status/", key),
                          list(output = TRUE))
  expect_equal(res2_api$status, 200L)
  expect_equal(res2_api$headers[["Content-Type"]], "application/json")
  expect_equal(res2_api$body, as.character(res2_endpoint$body))
})


test_that("status - completed", {
  runner <- test_runner2()
  key <- runner$queue("example")
  wait_for_finished_runner(runner, key)
  dat <- runner$status(key, TRUE)

  res1 <- target_status(runner, key)
  res2 <- target_status(runner, key, TRUE)
  expect_equal(
    res1,
    list(key = scalar(key),
         status = scalar("success"),
         version = scalar(dat$id),
         output = NA))
  expect_equal(
    res2,
    list(key = scalar(key),
         status = scalar("success"),
         version = scalar(dat$id),
         output = dat$output))

  ## endpoint
  endpoint <- endpoint_status(runner)
  res1_endpoint <- endpoint$run(key)
  expect_equal(res1_endpoint$status_code, 200)
  expect_equal(res1_endpoint$content_type, "application/json")
  expect_equal(res1_endpoint$data, res1)

  res2_endpoint <- endpoint$run(key, TRUE)
  expect_equal(res2_endpoint$status_code, 200)
  expect_equal(res2_endpoint$content_type, "application/json")
  expect_equal(res2_endpoint$data, res2)

  ## api
  api <- pkgapi::pkgapi$new()$handle(endpoint)
  res1_api <- api$request("GET", sprintf("/v1/reports/%s/status/", key))
  expect_equal(res1_api$status, 200L)
  expect_equal(res1_api$headers[["Content-Type"]], "application/json")
  expect_equal(res1_api$body, as.character(res1_endpoint$body))

  res2_api <- api$request("GET", sprintf("/v1/reports/%s/status/", key),
                          list(output = TRUE))
  expect_equal(res2_api$status, 200L)
  expect_equal(res2_api$headers[["Content-Type"]], "application/json")
  expect_equal(res2_api$body, as.character(res2_endpoint$body))
})
