context("orderly.server")

test_that("root", {
  server <- start_test_server()
  on.exit(server$stop())

  r <- httr::GET(server$api_url("/"))
  dat <- content(r)
  expect_equal(dat$status, "success")
  expect_equal(dat$errors, NULL)
  expect_is(dat$data$endpoints, "character")
})


test_that("rebuild", {
  server <- start_test_server()
  on.exit(server$stop())

  r <- httr::POST(server$api_url("/v1/reports/rebuild/"))
  dat <- content(r)
  expect_equal(dat$status, "success")
  expect_null(dat$data)
  expect_equal(dat$errors, NULL)
})


test_that("error handling: invalid method", {
  server <- start_test_server()
  on.exit(server$stop())

  r <- httr::GET(server$api_url("/v1/reports/rebuild/"))
  expect_equal(httr::status_code(r), 404L)
  dat <- content(r)
  expect_equal(dat$status, "failure")
  expect_equal(length(dat$errors), 1L)
  expect_equal(dat$errors[[1]]$error, "NOT_FOUND")
  expect_is(dat$errors[[1]]$detail, "character")
})


test_that("error handling: invalid url", {
  server <- start_test_server()
  on.exit(server$stop())

  r <- httr::GET(server$api_url("/v1/reports/rebuild"))
  expect_equal(httr::status_code(r), 404L)
  dat <- content(r)
  expect_equal(dat$status, "failure")
  expect_equal(length(dat$errors), 1L)
  expect_equal(dat$errors[[1]]$error, "NOT_FOUND")
  expect_is(dat$errors[[1]]$detail, "character")
})


test_that("run", {
  server <- start_test_server()
  on.exit(server$stop())

  r <- httr::POST(server$api_url("/v1/reports/example/run/"),
                  body = NULL, encode = "json")
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
  r <- httr::GET(server$api_url(dat$data$path))
  expect_equal(httr::status_code(r), 200)
  st <- content(r)
  expect_equal(st$status, "success")
  expect_is(st$data, "list")
  id <- st$data$version

  dest <- file.path(server$path, "archive", "example", id)
  wait_for_path(dest)
  wait_for_finished(dat$data$key, server)

  r <- httr::GET(server$api_url(dat$data$path))
  expect_equal(httr::status_code(r), 200)
  st <- content(r)
  expect_equal(st$status, "success")
  cmp <- list(key = dat$data$key, status = "success",
              version = id, output = NULL)
  expect_equal(st$data, cmp)

  r <- httr::GET(server$api_url(dat$data$path), query = list(output = TRUE))
  expect_equal(httr::status_code(r), 200)
  st <- content(r)
  expect_equal(st$status, "success")
  expect_is(st$data$output$stderr, "character")
  expect_equal(length(st$data$output$stdout), 0)
})


test_that("git", {
  path <- orderly_prepare_orderly_git_example()
  server <- start_test_server(path[["local"]])
  on.exit(server$stop())

  sha <- vapply(path, orderly_git_ref_to_sha, "", ref = "HEAD")

  r <- content(httr::GET(server$api_url("/v1/reports/git/status/")))
  expect_equal(r$data$hash, sha[["local"]])

  r <- httr::POST(server$api_url("/v1/reports/minimal/run/?update=false"))
  dat <- content(r)
  wait_for_finished(dat$data$key, server)
  expect_equal(orderly_git_ref_to_sha("HEAD", path[["local"]]),
               sha[["local"]])

  r <- httr::POST(server$api_url("/v1/reports/minimal/run/"),
                  query = list(update = "false", ref = sha[["origin"]]))
  dat <- content(r)
  wait_for_finished(dat$data$key, server)

  r <- httr::GET(server$api_url(dat$data$path))
  st <- content(r)
  expect_equal(httr::status_code(r), 200)
  expect_equal(st$data$status, "error")

  expect_equal(orderly_git_ref_to_sha("HEAD", root = path[["local"]]),
               sha[["local"]])
  expect_false(orderly_git_ref_exists(sha[["origin"]], path[["local"]]))

  r <- httr::POST(server$api_url("/v1/reports/minimal/run/"),
                  query = list(ref = sha[["origin"]]))
  dat <- content(r)
  wait_for_finished(dat$data$key, server)

  res <- content(httr::GET(server$api_url(content(r)$data$path),
                           query = list(output = TRUE)))
  expect_match(res$data$output$stderr, sha[["origin"]], all = FALSE)

  expect_equal(orderly_git_ref_to_sha("HEAD", root = path[["local"]]),
               sha[["local"]])
  expect_true(orderly_git_ref_exists(sha[["origin"]], path[["local"]]))
})


test_that("git error returns valid json", {
  path <- orderly_prepare_orderly_git_example()
  server <- start_test_server(path[["local"]])
  on.exit(server$stop())

  orderly_git_run(c("remote", "remove", "origin"), root = path[["local"]])

  r <- content(httr::GET(server$api_url("/v1/reports/git/status/")))
  res <- httr::POST(server$api_url("/v1/reports/git/fetch/"))
  json <- httr::content(res, "text", encoding = "UTF-8")
})


test_that("run report honours timeout", {
  server <- start_test_server()
  on.exit(server$stop())

  p <- file.path(server$path, "src", "count", "parameters.json")
  writeLines(jsonlite::toJSON(list(time = 2, poll = 0.1), auto_unbox = TRUE),
             p)

  r <- httr::POST(server$api_url("/v1/reports/count/run/"),
                  query = list(timeout = 1))
  expect_equal(httr::status_code(r), 200)
  dat <- content(r)
  wait_for_finished(dat$data$key, server)
  r <- httr::GET(server$api_url(dat$data$path))
  expect_equal(httr::status_code(r), 200)
  st <- content(r)
  expect_equal(st$data$status, "killed")

  r <- httr::POST(server$api_url("/v1/reports/count/run/"),
                  query = list(timeout = 60))
  expect_equal(httr::status_code(r), 200)
  dat <- content(r)
  wait_for_finished(dat$data$key, server)
  r <- httr::GET(server$api_url(dat$data$path))
  expect_equal(httr::status_code(r), 200)
  st <- content(r)
  expect_equal(st$data$status, "success")
})


test_that("pass parameters", {
  server <- start_test_server()
  on.exit(server$stop())

  r <- httr::POST(server$api_url("/v1/reports/count_param/run/"),
                  query = list(timeout = 60),
                  body = list(time = 1, poll = 0.1),
                  encode = "json")

  expect_equal(httr::status_code(r), 200)
  dat <- content(r)
  expect_equal(dat$status, "success")
  expect_is(dat$data, "list")

  expect_true(setequal(names(dat$data), c("name", "key", "path")))
  expect_equal(dat$data$name, "count_param")
  expect_is(dat$data$key, "character")
  expect_is(dat$data$path, "character")

  ## Then we ask about status
  wait_for_id(dat$data$key, server)
  r <- httr::GET(server$api_url(dat$data$path))
  expect_equal(httr::status_code(r), 200)
  st <- content(r)
  expect_equal(st$status, "success")
  expect_is(st$data, "list")
  id <- st$data$version

  dest <- file.path(server$path, "archive", "count_param", id)
  wait_for_path(dest)
  wait_for_finished(dat$data$key, server)

  r <- httr::GET(server$api_url(dat$data$path))
  expect_equal(httr::status_code(r), 200)
  st <- content(r)
  expect_equal(st$status, "success")
  cmp <- list(key = dat$data$key, status = "success",
              version = id, output = NULL)
  expect_equal(st$data, cmp)

  r <- httr::GET(server$api_url(dat$data$path), query = list(output = TRUE))
  expect_equal(httr::status_code(r), 200)
  st <- content(r)
  expect_equal(st$status, "success")
  expect_is(st$data$output$stderr, "character")
  expect_equal(length(st$data$output$stdout), 0)

  ## parameters make it across
  expect_match(st$data$output$stderr, "time: 1", fixed = TRUE, all = FALSE)
  expect_match(st$data$output$stderr, "poll: 0.1", fixed = TRUE, all = FALSE)
})

test_that("run-metadata", {
  path <- orderly_prepare_orderly_git_example()
  server <- start_test_server(path[["local"]])
  on.exit(server$stop())

  r <- content(httr::GET(server$api_url("/run-metadata")))

  expect_equal(r$status, "success")
  expect_null(r$errors)
  expect_equal(names(r$data), c("name", "instances_supported", "git_supported",
                                "instances", "changelog_types"))
  expect_null(r$data$name)
  expect_false(r$data$instances_supported)
  expect_true(r$data$git_supported)
  expect_equal(r$data$instances, list(source = list()))
  expect_equal(r$data$changelog_types, c(scalar("public")))
})

test_that("git/branches", {
  path <- orderly_prepare_orderly_git_example()
  server <- start_test_server(path[["local"]])
  on.exit(server$stop())

  r <- content(httr::GET(server$api_url("/git/branches")))

  expect_equal(r$status, "success")
  expect_null(r$errors)
  expect_length(r$data, 2)
  expect_equal(names(r$data[[1]]), c("name", "last_commit", "last_commit_age"))
  expect_equal(r$data[[1]]$name, "master")
  expect_equal(r$data[[2]]$name, "other")
})

test_that("git/commits", {
  path <- orderly_prepare_orderly_git_example()
  server <- start_test_server(path[["local"]])
  on.exit(server$stop())

  r <- content(httr::GET(server$api_url("/git/commits?branch=master")))

  expect_equal(r$status, "success")
  expect_null(r$errors)
  expect_length(r$data, 1)
  expect_equal(names(r$data[[1]]), c("id", "date_time", "age"))
  expect_type(r$data[[1]]$id, "character")
  expect_match(r$data[[1]]$date_time,
               "^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}$")
  expect_type(r$data[[1]]$age, "integer")

  other_r <- content(httr::GET(server$api_url("/git/commits?branch=other")))

  expect_equal(other_r$status, "success")
  expect_null(other_r$errors)
  expect_length(other_r$data, 1)
  expect_true(other_r$data[[1]]$id != r$data[[1]]$id)
})

test_that("can get available reports", {
  path <- orderly_prepare_orderly_git_example()
  server <- start_test_server(path[["local"]])
  on.exit(server$stop())

  r <- content(httr::GET(server$api_url("/git/commits?branch=master")))
  expect_equal(r$status, "success")

  url <- paste0("/reports/source?branch=master&commit=", r$data[[1]]$id)
  reports <- content(httr::GET(server$api_url(url)))
  expect_equal(reports$status, "success")
  expect_equal(reports$data, c("global", "minimal"))

  other_r <- content(httr::GET(server$api_url("/git/commits?branch=other")))
  expect_equal(other_r$status, "success")

  url <- paste0("/reports/source?branch=other&commit=", other_r$data[[1]]$id)
  other_reports <- content(httr::GET(server$api_url(url)))
  expect_equal(other_reports$status, "success")
  expect_equal(other_reports$data, "other")
})

test_that("can get report parameters", {
  path <- orderly_prepare_orderly_git_example()
  server <- start_test_server(path[["local"]])
  on.exit(server$stop())

  r <- content(httr::GET(server$api_url("/git/commits?branch=master")))
  expect_equal(r$status, "success")

  url <- paste0("/reports/minimal/parameters?commit=", r$data[[1]]$id)
  params <- content(httr::GET(server$api_url(url)))
  expect_equal(params$status, "success")
  expect_equal(params$data, list())
  expect_equal(params$errors, NULL)

  other_r <- content(httr::GET(server$api_url("/git/commits?branch=other")))
  expect_equal(other_r$status, "success")

  url <- paste0("/reports/other/parameters?commit=", other_r$data[[1]]$id)
  other_params <- content(httr::GET(server$api_url(url)))
  expect_equal(other_params$status, "success")
  expect_equal(other_params$data, list(
    list(name = "nmin",
         default = NULL)
  ))
})
