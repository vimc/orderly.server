context("spec")

test_that("response: success", {
  skip("old spec")
  json <- server_response(NULL, list(), 200)$body
  expect_valid_json(json, "spec/Response.schema.json")
})


test_that("response: error", {
  skip("old spec")
  dat <- server_error_data("my-code", "my description", 400)
  json <- server_response(NA, dat$errors, dat$status)$body
  expect_valid_json(json, "spec/Response.schema.json")
})


test_that("orderly id", {
  skip("old spec")
  id <- "20170912-075922-332ab657"
  expect_valid_json(to_json(id), "spec/OrderlyId.schema.json")
})


test_that("index", {
  skip("old spec")
  res <- test_runner()
  data <- res$index$dest()
  expect_valid_json(to_json(data), "spec/Index.schema.json")
})


test_that("rebuild", {
  skip("old spec")
  res <- test_runner()
  data <- res$rebuild$dest()
  json <- to_json(data)
  expect_valid_json(json, "spec/Rebuild.schema.json")
})


test_that("run", {
  skip("old spec")
  res <- test_runner()
  data <- res$run$dest("example")

  json <- to_json(data)
  expect_valid_json(json, "spec/Run.schema.json")
})


test_that("status", {
  skip("old spec")
  skip_on_travis()
  path <- tempfile()
  res <- test_runner(path)

  info <- res$run$dest("example")

  runner <- environment(res$index$dest)$runner

  data <- res$status$dest(info$key)

  expect_equal(runner$poll(), structure("create", key = info$key))
  wait_for_path(file.path(runner$path_id, info$key))
  id <- readLines(file.path(runner$path_id, info$key))
  wait_for_process_termination(runner$process$px)
  expect_equal(runner$poll(), structure("finish", key = info$key))

  dat1 <- res$status$dest(info$key, FALSE)
  dat2 <- res$status$dest(info$key, TRUE)
  expect_valid_json(to_json(dat1), "spec/Status.schema.json")
  expect_valid_json(to_json(dat2), "spec/Status.schema.json")
})


test_that("kill", {
  skip("old spec")
  skip_on_travis()
  path <- tempfile()
  res <- test_runner(path)

  info <- res$run$dest("interactive")

  runner <- environment(res$index$dest)$runner

  data <- res$status$dest(info$key)

  expect_equal(runner$poll(), structure("create", key = info$key))
  wait_for_path(file.path(runner$path_id, info$key))
  id <- readLines(file.path(runner$path_id, info$key))

  dat1 <- res$kill$dest(info$key)
  expect_valid_json(to_json(dat1), "spec/Kill.schema.json")
  runner$poll()

  expect_error(res$kill$dest(info$key), "Can't kill")
})


test_that("publish", {
  skip("old spec")
  path <- tempfile()
  res <- test_runner(path)

  id <- orderly::orderly_run("example", root = path, echo = FALSE)
  orderly::orderly_commit(id, "example", root = path)

  data <- res$publish$dest("example", id)
  json <- to_json(data)
  expect_valid_json(json, "spec/Publish.schema.json")

  data <- res$publish$dest("example", id, FALSE)
  json <- to_json(data)
  expect_valid_json(json, "spec/Publish.schema.json")
})


test_that("git bits", {
  skip("old spec")
  git_run <- orderly:::git_run
  path1 <- orderly:::unzip_git_demo()
  path2 <- tempfile()
  git_run(c("clone", "--", path1, path2), check = TRUE)
  writeLines("new", file.path(path1, "new"))
  git_run(c("add", "."), path1)
  git_run(c("commit", "-m", "orderly"), path1)

  sha1 <- orderly:::git_ref_to_sha("HEAD", path1)
  sha2 <- orderly:::git_ref_to_sha("HEAD", path2)

  runner <- server_endpoints(orderly::orderly_runner(path2))

  data <- runner$git_status$dest()
  expect_equal(data$hash, sha2)
  expect_valid_json(to_json(data), "spec/GitStatus.schema.json")

  data <- runner$git_fetch$dest()
  expect_valid_json(to_json(data), "spec/GitFetch.schema.json")

  data <- runner$git_pull$dest()
  expect_valid_json(to_json(data), "spec/GitPull.schema.json")

  data <- runner$git_status$dest()
  expect_equal(data$hash, sha1)
})
