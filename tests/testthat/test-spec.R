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

  runner <- environment(res$index$dest)$runner

  data <- res$status$dest(info$key)

  expect_equal(runner$poll(), "create")
  wait_for_path(file.path(runner$path_id, info$key))
  id <- readLines(file.path(runner$path_id, info$key))
  wait_for_process_termination(runner$process$px)
  expect_equal(runner$poll(), "finish")

  dat1 <- res$status$dest(info$key, FALSE)
  dat2 <- res$status$dest(info$key, TRUE)
  expect_valid_json(to_json(dat1), "spec/Status.schema.json")
  expect_valid_json(to_json(dat2), "spec/Status.schema.json")
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
