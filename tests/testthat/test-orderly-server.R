context("orderly.server")

test_that("root", {
  r <- httr::GET(api_url("/"))
  dat <- content(r)
  expect_equal(dat$status, "success")
  expect_equal(dat$errors, list())
  expect_is(dat$data$endpoints, "character")
})

test_that("rebuild", {
  r <- httr::POST(api_url("/v1/reports/rebuild/"))
  dat <- content(r)
  expect_equal(dat$status, "success")
  expect_null(dat$data)
  expect_equal(dat$errors, list())
})

test_that("error handling: invalid method", {
  r <- httr::GET(api_url("/v1/reports/rebuild/"))
  expect_equal(httr::status_code(r), 405L)
  dat <- content(r)
  expect_equal(dat$status, "failure")
  expect_equal(length(dat$errors), 1L)
  expect_equal(dat$errors[[1]]$code, "invalid-method")
  expect_is(dat$errors[[1]]$message, "character")
})

test_that("error handling: invalid url", {
  r <- httr::GET(api_url("/v1/reports/rebuild"))
  expect_equal(httr::status_code(r), 404L)
  dat <- content(r)
  expect_equal(dat$status, "failure")
  expect_equal(length(dat$errors), 1L)
  expect_equal(dat$errors[[1]]$code, "unknown-endpoint")
  expect_is(dat$errors[[1]]$message, "character")
})

test_that("run", {
  r <- httr::POST(api_url("/v1/reports/example/run/"))
  expect_equal(httr::status_code(r), 200)
  dat <- content(r)
  expect_equal(dat$status, "success")
  expect_is(dat$data, "list")

  expect_true(setequal(names(dat$data), c("name", "key", "path")))
  expect_equal(dat$data$name, "example")
  expect_is(dat$data$key, "character")
  expect_is(dat$data$path, "character")

  ## Then we ask about status
  wait_for_id(dat$data$key)
  r <- httr::GET(api_url(dat$data$path))
  expect_equal(httr::status_code(r), 200)
  st <- content(r)
  expect_equal(st$status, "success")
  expect_is(st$data, "list")
  id <- st$data$version

  dest <- file.path(cache$server$path, "archive", "example", id)
  wait_for_path(dest)
  wait_for_finished(dat$data$key)

  r <- httr::GET(api_url(dat$data$path))
  expect_equal(httr::status_code(r), 200)
  st <- content(r)
  expect_equal(st$status, "success")
  cmp <- list(key = dat$data$key, status = "success",
              version = id, output = NULL)
  expect_equal(st$data, cmp)

  r <- httr::GET(api_url(dat$data$path), query = list(output = TRUE))
  expect_equal(httr::status_code(r), 200)
  st <- content(r)
  expect_equal(st$status, "success")
  expect_is(st$data$output$stderr, "character")
  expect_is(st$data$output$stdout, "character")
})

test_that("publish", {
  path <- cache$server$path
  id <- orderly::orderly_run("example", config = path, echo = FALSE)
  ## This is somewhat liable to failure due to db locking
  dest <- orderly::orderly_commit(id, config = path)
  pub <- file.path(dest, "orderly_published.yml")

  r <- httr::POST(api_url("/v1/reports/example/%s/publish/", id))
  expect_equal(httr::status_code(r), 200)
  dat <- content(r)
  expect_true(dat$data)

  expect_true(file.exists(pub))
  expect_equal(orderly:::yaml_read(pub), list(published = TRUE))

  r <- httr::POST(api_url("/v1/reports/example/%s/publish/", id),
                  query = list(value = TRUE))
  expect_equal(httr::status_code(r), 200)
  dat <- content(r)
  expect_true(dat$data)
  expect_equal(orderly:::yaml_read(pub), list(published = TRUE))

  r <- httr::POST(api_url("/v1/reports/example/%s/publish/", id),
                  query = list(value = FALSE))
  expect_equal(httr::status_code(r), 200)
  dat <- content(r)
  expect_false(dat$data)
  expect_equal(orderly:::yaml_read(pub), list(published = FALSE))
})
