context("orderly.server")

test_that("rebuild", {
  r <- httr::POST(api_url("/reports/rebuild"))
  dat <- content(r)
  expect_equal(dat$status, "success")
  expect_equal(dat$data, empty_named_list())
  expect_equal(dat$errors, list())
})

test_that("error handling: invalid method", {
  r <- httr::GET(api_url("/reports/rebuild"))
  expect_equal(httr::status_code(r), 405L)
  dat <- content(r)
  expect_equal(dat$status, "failure")
  expect_equal(length(dat$errors), 1L)
  expect_equal(dat$errors[[1]]$code, "invalid-method")
  expect_is(dat$errors[[1]]$message, "character")
})

test_that("error handling: invalid url", {
  r <- httr::GET(api_url("/reports/rebuild2"))
  expect_equal(httr::status_code(r), 404L)
  dat <- content(r)
  expect_equal(dat$status, "failure")
  expect_equal(length(dat$errors), 1L)
  expect_equal(dat$errors[[1]]$code, "unknown-endpoint")
  expect_is(dat$errors[[1]]$message, "character")
})

test_that("run", {
  r <- httr::POST(api_url("/reports/example/run"))
  expect_equal(httr::status_code(r), 200)
  dat <- content(r)
  expect_equal(dat$status, "success")
  expect_is(dat$data, "character")
  id <- dat$data

  path <- readLines("orderly.server.path")

  dest <- file.path(path, "archive", "example", id)
  wait_for_path(dest)

  r <- httr::GET(api_url("/reports/example/%s/status", id))
  expect_equal(httr::status_code(r), 200)
  dat <- content(r)
  expect_equal(dat$status, "success")
  expect_equal(dat$data,
               list(status = "archive", output = empty_named_list()))

  r <- httr::GET(api_url("/reports/example/%s/status", id),
                 query = list(output = TRUE))
  expect_equal(httr::status_code(r), 200)
  dat <- content(r)
  expect_equal(dat$status, "success")
  expect_is(dat$data$output$stderr, "character")
  expect_is(dat$data$output$stdout, "character")
})

test_that("run, commit", {
  path <- readLines("orderly.server.path")

  r <- httr::POST(api_url("/reports/example/run"), query = list(commit = FALSE))
  expect_equal(httr::status_code(r), 200)
  dat <- content(r)
  expect_equal(dat$status, "success")
  expect_is(dat$data, "character")
  id <- dat$data

  dest <- file.path(path, "draft", "example", id)
  wait_for_path(file.path(dest, "orderly_run.yml"))

  r <- httr::GET(api_url("/reports/example/%s/status", id))
  expect_equal(httr::status_code(r), 200)
  dat <- content(r)
  expect_equal(dat$status, "success")
  expect_equal(dat$data,
               list(status = "draft", output = empty_named_list()))

  r <- httr::POST(api_url("/reports/example/%s/commit", id))
  expect_equal(httr::status_code(r), 200)
  dat <- content(r)
  expect_equal(dat$status, "success")
  expect_equal(dat$data, TRUE)
})

test_that("publish", {
  path <- readLines("orderly.server.path")
  id <- orderly::orderly_run("example", config = path, echo = FALSE)
  dest <- orderly::orderly_commit(id, config = path)
  pub <- file.path(dest, "orderly_published.yml")

  r <- httr::POST(api_url("/reports/example/%s/publish", id))
  expect_equal(httr::status_code(r), 200)
  dat <- content(r)
  expect_true(dat$data)

  expect_true(file.exists(pub))
  expect_equal(orderly:::yaml_read(pub), list(published = TRUE))

  r <- httr::POST(api_url("/reports/example/%s/publish", id),
                  query = list(value = TRUE))
  expect_equal(httr::status_code(r), 200)
  dat <- content(r)
  expect_true(dat$data)
  expect_equal(orderly:::yaml_read(pub), list(published = TRUE))

  r <- httr::POST(api_url("/reports/example/%s/publish", id),
                  query = list(value = FALSE))
  expect_equal(httr::status_code(r), 200)
  dat <- content(r)
  expect_false(dat$data)
  expect_equal(orderly:::yaml_read(pub), list(published = FALSE))
})
