context("spec")

test_that("response: success", {
  json <- server_response(NULL, list(), 200)$body
  expect_valid_json(json, "spec/Response.schema.json")
})

test_that("response: error", {
  dat <- server_error_data("my-code", "my description", 400)
  json <- server_response(NA, dat$errors, dat$status)$body
  expect_valid_json(json, "spec/Response.schema.json")
})

test_that("orderly id", {
  id <- "20170912-075922-332ab657"
  expect_valid_json(to_json(id), "spec/OrderlyId.schema.json")
})

test_that("index", {
  res <- test_runner()
  data <- res$index$dest()
  expect_valid_json(to_json(data), "spec/Index.schema.json")
})

test_that("rebuild", {
  res <- test_runner()
  data <- res$rebuild$dest()
  json <- to_json(data)
  expect_valid_json(json, "spec/Rebuild.schema.json")
})

test_that("run", {
  res <- test_runner()
  data <- res$run$dest("example")

  json <- to_json(data)
  expect_valid_json(json, "spec/Run.schema.json")
})

test_that("status", {
  path <- tempfile()
  res <- test_runner(path)

  info <- res$run$dest("example")

  dest <- file.path(path, "archive", "example", info$version)
  wait_for_path(dest)

  ## No output:
  data <- res$status$dest(info$name, info$version, FALSE)
  json <- to_json(data)
  expect_valid_json(json, "spec/Status.schema.json")

  ## With output:
  data <- res$status$dest(info$name, info$version, TRUE)
  json <- to_json(data)
  expect_valid_json(json, "spec/Status.schema.json")
})

test_that("commit", {
  path <- tempfile()
  res <- test_runner(path)

  id <- orderly::orderly_run("example", config = path, echo = FALSE)

  data <- res$commit$dest("example", id)
  json <- to_json(data)
  expect_valid_json(json, "spec/Commit.schema.json")
})

test_that("publish", {
  path <- tempfile()
  res <- test_runner(path)

  id <- orderly::orderly_run("example", config = path, echo = FALSE)
  orderly::orderly_commit(id, "example", config = path)

  data <- res$publish$dest("example", id)
  json <- to_json(data)
  expect_valid_json(json, "spec/Publish.schema.json")

  data <- res$publish$dest("example", id, FALSE)
  json <- to_json(data)
  expect_valid_json(json, "spec/Publish.schema.json")
})
