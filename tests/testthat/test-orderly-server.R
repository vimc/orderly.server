context("orderly.server")

test_that("root", {
  server <- start_test_server()
  on.exit(stop_test_server(server))

  r <- httr::GET(server$url("/"))
  dat <- content(r)
  expect_equal(dat$status, "success")
  expect_equal(dat$errors, list())
  expect_is(dat$data$endpoints, "character")
})

test_that("rebuild", {
  server <- start_test_server()
  on.exit(stop_test_server(server))

  r <- httr::POST(server$url("/v1/reports/rebuild/"))
  dat <- content(r)
  expect_equal(dat$status, "success")
  expect_null(dat$data)
  expect_equal(dat$errors, list())
})

test_that("error handling: invalid method", {
  server <- start_test_server()
  on.exit(stop_test_server(server))

  r <- httr::GET(server$url("/v1/reports/rebuild/"))
  expect_equal(httr::status_code(r), 405L)
  dat <- content(r)
  expect_equal(dat$status, "failure")
  expect_equal(length(dat$errors), 1L)
  expect_equal(dat$errors[[1]]$code, "invalid-method")
  expect_is(dat$errors[[1]]$message, "character")
})

test_that("error handling: invalid url", {
  server <- start_test_server()
  on.exit(stop_test_server(server))

  r <- httr::GET(server$url("/v1/reports/rebuild"))
  expect_equal(httr::status_code(r), 404L)
  dat <- content(r)
  expect_equal(dat$status, "failure")
  expect_equal(length(dat$errors), 1L)
  expect_equal(dat$errors[[1]]$code, "unknown-endpoint")
  expect_is(dat$errors[[1]]$message, "character")
})

test_that("run", {
  server <- start_test_server()
  on.exit(stop_test_server(server))

  r <- httr::POST(server$url("/v1/reports/example/run/"))
  expect_equal(httr::status_code(r), 200)
  dat <- content(r)
  expect_equal(dat$status, "success")
  expect_is(dat$data, "list")

  expect_true(setequal(names(dat$data), c("name", "key", "path")))
  expect_equal(dat$data$name, "example")
  expect_is(dat$data$key, "character")
  expect_is(dat$data$path, "character")

  ## Then we ask about status
  wait_for_id(dat$data$key, server)
  r <- httr::GET(server$url(dat$data$path))
  expect_equal(httr::status_code(r), 200)
  st <- content(r)
  expect_equal(st$status, "success")
  expect_is(st$data, "list")
  id <- st$data$version

  dest <- file.path(server$path, "archive", "example", id)
  wait_for_path(dest)
  wait_for_finished(dat$data$key, server)

  r <- httr::GET(server$url(dat$data$path))
  expect_equal(httr::status_code(r), 200)
  st <- content(r)
  expect_equal(st$status, "success")
  cmp <- list(key = dat$data$key, status = "success",
              version = id, output = NULL)
  expect_equal(st$data, cmp)

  r <- httr::GET(server$url(dat$data$path), query = list(output = TRUE))
  expect_equal(httr::status_code(r), 200)
  st <- content(r)
  expect_equal(st$status, "success")
  expect_is(st$data$output$stderr, "character")
  expect_is(st$data$output$stdout, "character")
})

test_that("publish", {
  server <- start_test_server()
  on.exit(stop_test_server(server))

  path <- server$path
  id <- orderly::orderly_run("example", config = path, echo = FALSE)
  ## This is somewhat liable to failure due to db locking
  dest <- orderly::orderly_commit(id, config = path)
  pub <- file.path(dest, "orderly_published.yml")

  r <- httr::POST(server$url("/v1/reports/example/%s/publish/", id))
  expect_equal(httr::status_code(r), 200)
  dat <- content(r)
  expect_true(dat$data)

  expect_true(file.exists(pub))
  expect_equal(orderly:::yaml_read(pub), list(published = TRUE))

  r <- httr::POST(server$url("/v1/reports/example/%s/publish/", id),
                  query = list(value = TRUE))
  expect_equal(httr::status_code(r), 200)
  dat <- content(r)
  expect_true(dat$data)
  expect_equal(orderly:::yaml_read(pub), list(published = TRUE))

  r <- httr::POST(server$url("/v1/reports/example/%s/publish/", id),
                  query = list(value = FALSE))
  expect_equal(httr::status_code(r), 200)
  dat <- content(r)
  expect_false(dat$data)
  expect_equal(orderly:::yaml_read(pub), list(published = FALSE))
})

test_that("git", {
  path <- orderly:::prepare_orderly_git_example()
  server <- start_test_server(path[["local"]])
  on.exit(stop_test_server(server))

  sha <- vapply(path, orderly:::git_ref_to_sha, "", ref = "HEAD")

  r <- content(httr::GET(server$url("/v1/reports/git/status/")))
  expect_equal(r$data$hash, sha[["local"]])

  r <- httr::POST(server$url("/v1/reports/minimal/run/?update=false"))
  dat <- content(r)
  wait_for_finished(dat$data$key, server)

  expect_equal(orderly:::git_ref_to_sha("HEAD", path[["local"]]),
               sha[["local"]])

  r <- httr::POST(server$url("/v1/reports/minimal/run/"),
                  query = list(update = "false", ref = sha[["origin"]]))
  dat <- content(r)
  expect_true(httr::status_code(r) >= 400)
  expect_equal(dat$status, "failure")
  expect_null(dat$data)
  expect_match(dat$errors, "Did not find git reference")

  expect_equal(orderly:::git_ref_to_sha("HEAD", root = path[["local"]]),
               sha[["local"]])
  expect_false(orderly:::git_ref_exists(sha[["origin"]], path[["local"]]))

  r <- httr::POST(server$url("/v1/reports/minimal/run/"),
                  query = list(ref = sha[["origin"]]))
  dat <- content(r)
  wait_for_finished(dat$data$key, server)

  res <- content(httr::GET(server$url(content(r)$data$path),
                           query = list(output = TRUE)))
  expect_match(res$data$output$stderr, sha[["origin"]], all = FALSE)

  expect_equal(orderly:::git_ref_to_sha("HEAD", root = path[["local"]]),
               sha[["local"]])
  expect_true(orderly:::git_ref_exists(sha[["origin"]], path[["local"]]))
})
