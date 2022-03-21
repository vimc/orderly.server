context("orderly.server")

test_that("root", {
  server <- start_test_server()
  on.exit(server$stop())

  r <- httr::GET(server$api_url("/"))
  dat <- content(r)
  expect_equal(dat$status, "success")
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
  wait_for_version(dat$data$key, server)
  r <- httr::GET(server$api_url(dat$data$path))
  expect_equal(httr::status_code(r), 200)
  st <- content(r)
  expect_equal(st$status, "success")
  expect_is(st$data, "list")
  expect_type(st$data$start_time, "integer")
  id <- st$data$version

  dest <- file.path(server$path, "archive", "example", id)
  wait_for_path(dest)
  wait_for_finished(dat$data$key, server)

  r <- httr::GET(server$api_url(dat$data$path))
  expect_equal(httr::status_code(r), 200)
  st <- content(r)
  expect_equal(st$status, "success")
  expect_equal(st$data$key, dat$data$key)
  expect_equal(st$data$status, "success")
  expect_equal(st$data$version, id)
  expect_type(st$data$start_time, "integer")
  expect_null(st$data$output)
  expect_equal(st$data$queue, list())

  r <- httr::GET(server$api_url(dat$data$path), query = list(output = TRUE))
  expect_equal(httr::status_code(r), 200)
  st <- content(r)
  expect_equal(st$status, "success")
  expect_match(st$data$output, paste0("\\[ id +\\]  ", id),
               all = FALSE)
})


test_that("git", {
  path <- orderly_prepare_orderly_git_example()
  server <- start_test_server(path[["local"]])
  on.exit(server$stop())

  sha <- vapply(path, git_ref_to_sha, "", ref = "HEAD")

  r <- content(httr::GET(server$api_url("/v1/reports/git/status/")))
  expect_equal(r$data$hash, sha[["local"]])

  expect_equal(git_ref_to_sha("HEAD", root = path[["local"]]),
               sha[["local"]])
  expect_false(git_ref_exists(sha[["origin"]], path[["local"]]))

  ## When ref is specified git is fetched but HEAD not advanced
  r <- httr::POST(server$api_url("/v1/reports/minimal/run/"),
                  query = list(ref = sha[["origin"]]))
  dat <- content(r)
  wait_for_finished(dat$data$key, server)

  r <- httr::GET(server$api_url(dat$data$path))
  st <- content(r)
  expect_equal(httr::status_code(r), 200)
  expect_equal(st$data$status, "success")

  expect_equal(git_ref_to_sha("HEAD", root = path[["local"]]),
               sha[["local"]])
  expect_true(git_ref_exists(sha[["origin"]], path[["local"]]))

  ## When ref is NULL HEAD is advanced
  r <- httr::POST(server$api_url("/v1/reports/minimal/run/"))
  dat <- content(r)
  wait_for_finished(dat$data$key, server)

  res <- content(httr::GET(server$api_url(content(r)$data$path),
                           query = list(output = TRUE)))

  expect_equal(git_ref_to_sha("HEAD", root = path[["local"]]),
               sha[["origin"]])
  expect_true(git_ref_exists(sha[["origin"]], path[["local"]]))
})


test_that("git error returns valid json", {
  path <- orderly_prepare_orderly_git_example()
  server <- start_test_server(path[["local"]])
  on.exit(server$stop())

  git_run(c("remote", "remove", "origin"), root = path[["local"]])

  r <- content(httr::GET(server$api_url("/v1/reports/git/status/")))
  res <- httr::POST(server$api_url("/v1/reports/git/pull/"))
  json <- httr::content(res, "text", encoding = "UTF-8")
  content <- httr::content(res)

  expect_equal(content$status, "failure")
  expect_length(content$errors, 1)
  expect_valid_json(json, system.file("schema/response-failure.schema.json",
                                      package = "porcelain",
                                      mustWork = TRUE))
})


test_that("run report honours timeout", {
  server <- start_test_server()
  on.exit(server$stop())

  p <- file.path(server$path, "src", "count", "parameters.json")
  writeLines(jsonlite::toJSON(list(time = 3, poll = 0.1), auto_unbox = TRUE), p)

  r <- httr::POST(server$api_url("/v1/reports/count/run/"),
                  query = list(timeout = 1))
  expect_equal(httr::status_code(r), 200)
  dat <- content(r)
  wait_for_finished(dat$data$key, server)
  r <- httr::GET(server$api_url(dat$data$path))
  expect_equal(httr::status_code(r), 200)
  st <- content(r)
  expect_equal(st$data$status, "interrupted")

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


test_that("run: pass parameters", {
  server <- start_test_server()
  on.exit(server$stop())

  r <- httr::POST(server$api_url("/v1/reports/count_param/run/"),
                  query = list(timeout = 60),
                  body = list(params = list(time = 1, poll = 0.1)),
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
  wait_for_version(dat$data$key, server)
  r <- httr::GET(server$api_url(dat$data$path))
  expect_equal(httr::status_code(r), 200)
  st <- content(r)
  expect_equal(st$status, "success")
  expect_is(st$data, "list")
  version <- st$data$version

  dest <- file.path(server$path, "archive", "count_param", version)
  wait_for_path(dest)
  wait_for_finished(dat$data$key, server)

  r <- httr::GET(server$api_url(dat$data$path))
  expect_equal(httr::status_code(r), 200)
  st <- content(r)
  expect_equal(st$status, "success")
  expect_equal(st$data$key, dat$data$key)
  expect_equal(st$data$status, "success")
  expect_equal(st$data$version, version)
  expect_type(st$data$start_time, "integer")
  expect_null(st$data$output)
  expect_equal(st$data$queue, list())

  r <- httr::GET(server$api_url(dat$data$path), query = list(output = TRUE))
  expect_equal(httr::status_code(r), 200)
  st <- content(r)
  expect_equal(st$status, "success")
  expect_match(st$data$output, paste0("\\[ id +\\]  ", version),
               all = FALSE)

  ## parameters make it across
  expect_match(st$data$output, "time: 1", fixed = TRUE, all = FALSE)
  expect_match(st$data$output, "poll: 0.1", fixed = TRUE, all = FALSE)
})

test_that("run: changelog", {
  path <- orderly_prepare_orderly_git_example()
  server <- start_test_server(path[["local"]])
  on.exit(server$stop())

  r <- httr::POST(server$api_url("/v1/reports/minimal/run/"),
                  query = list(timeout = 60),
                  body = list(changelog = list(type = "internal",
                                               message = "test")),
                  encode = "json")

  expect_equal(httr::status_code(r), 200)
  dat <- content(r)
  expect_equal(dat$status, "success")
  expect_is(dat$data, "list")

  expect_true(setequal(names(dat$data), c("name", "key", "path")))
  expect_equal(dat$data$name, "minimal")

  ## Then we ask about status
  wait_for_version(dat$data$key, server)
  r <- httr::GET(server$api_url(dat$data$path))
  expect_equal(httr::status_code(r), 200)
  st <- content(r)
  expect_equal(st$status, "success")
  expect_is(st$data, "list")
  version <- st$data$version

  dest <- file.path(server$path, "archive", "minimal", version)
  wait_for_path(dest)
  wait_for_finished(dat$data$key, server)

  d <- readRDS(orderly_path_orderly_run_rds(
    file.path(server$path, "archive", "minimal", version)))
  expect_true(!is.null(d$meta$changelog))
  expect_equal(d$meta$changelog$label, "internal")
  expect_equal(d$meta$changelog$value, "test")
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
  expect_equal(r$data$changelog_types, c(scalar("internal"), scalar("public")))
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
  expect_true("other" %in% other_reports$data)
  expect_true(!(any(c("global", "minimal") %in% other_reports$data)))
})

test_that("can get all reports on a branch", {
  path <- orderly_prepare_orderly_git_example()
  server <- start_test_server(path[["local"]])
  on.exit(server$stop())

  other_r <- content(httr::GET(server$api_url("/git/commits?branch=other")))
  expect_equal(other_r$status, "success")

  url <- paste0("/reports/source?branch=other&show_all=true&commit=",
                other_r$data[[1]]$id)
  other_reports <- content(httr::GET(server$api_url(url)))
  expect_equal(other_reports$status, "success")
  expect_true(all(c("global", "minimal", "other") %in% other_reports$data))

  ## Without a commit
  url <- paste0("/reports/source?branch=other&show_all=true")
  other_reports <- content(httr::GET(server$api_url(url)))
  expect_equal(other_reports$status, "success")
  expect_true(all(c("global", "minimal", "other") %in% other_reports$data))
})

test_that("can get available reports without parameters", {
  path <- orderly_prepare_orderly_git_example()
  server <- start_test_server(path[["local"]])
  on.exit(server$stop())

  reports <- content(httr::GET(server$api_url("/reports/source")))
  expect_equal(reports$status, "success")
  expect_true(all(c("global", "minimal") %in% reports$data))
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
         value = NULL)
  ))
})

test_that("can get report parameters with no commit ID", {
  path <- orderly_prepare_orderly_example("interactive", testing = TRUE,
                                          git = TRUE)
  server <- start_test_server(path)
  on.exit(server$stop())

  params <- content(httr::GET(server$api_url(
    "/reports/count_param/parameters")))
  expect_equal(params$status, "success")
  expect_equal(params$data, list(
    list(name = "time",
         value = NULL),
    list(name = "poll",
         value = NULL)
  ))
})


test_that("Can pack, run and import a bundle", {
  server <- start_test_server()
  on.exit(server$stop())

  res <- httr::POST(server$api_url("/v1/bundle/pack/example"))
  expect_equal(httr::status_code(res), 200L)
  zip_in <- tempfile()
  writeBin(httr::content(res, "raw"), zip_in)

  filename <- httr::headers(res)[["Content-Disposition"]]
  expect_match(filename, "^[0-9]{8}-[0-9]{6}-[[:xdigit:]]{8}\\.zip")

  ans <- orderly::orderly_bundle_run(zip_in, echo = FALSE)
  expect_equal(filename, paste0(ans$id, ".zip"))

  res_up <- httr::POST(server$api_url("/v1/bundle/import"),
    body = httr::upload_file(ans$path, "application/octet-stream"))
  expect_equal(httr::status_code(res), 200L)
  dat <- content(res_up)
  expect_equal(dat$status, "success")
  expect_equal(dat$errors, NULL)
  expect_true(dat$data)

  expect_equal(
    orderly::orderly_list_archive(server$path),
    data.frame(name = "example", id = ans$id, stringsAsFactors = FALSE))
})


test_that("Can create a report with parameters", {
  server <- start_test_server()
  on.exit(server$stop())

  res <- httr::POST(server$api_url("/v1/bundle/pack/count_param"),
                    body = list(time = 10, poll = 1), encode = "json")
  expect_equal(httr::status_code(res), 200L)
  zip_in <- tempfile()
  writeBin(httr::content(res, "raw"), zip_in)

  filename <- httr::headers(res)[["Content-Disposition"]]
  id <- sub("\\.zip$", "", filename)

  tmp <- tempfile()
  zip::unzip(zip_in, exdir = tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  expect_mapequal(
    readRDS(file.path(tmp, id, "meta", "info.rds"))$parameters,
    list(time = 10, poll = 1))
})

test_that("Can get dependencies", {
  server <- start_test_server()
  on.exit(server$stop())
  r <- httr::GET(server$api_url("/v1/reports/count/dependencies/"),
                  query = list(direction = "upstream", use = "src"))
  dat <- content(r)
  expect_equal(httr::status_code(r), 200)
  expect_equal(dat$data$direction, "upstream")
  dep_tree <- dat$data$dependency_tree
  expect_equal(dep_tree$name, "count")
  expect_equal(dep_tree$id, "latest")
  expect_equal(dep_tree$out_of_date, FALSE)
  expect_equal(dep_tree$dependencies, list())
})

test_that("can get report info", {
  server <- start_test_server()
  on.exit(server$stop())

  ## Run a report first to retrieve info for
  r <- httr::POST(server$api_url("/v1/reports/count_param/run/"),
                  query = list(timeout = 60),
                  body = list(params = list(time = 1, poll = 0.1)),
                  encode = "json")

  expect_equal(httr::status_code(r), 200)
  dat <- content(r)
  expect_equal(dat$status, "success")
  expect_is(dat$data, "list")

  wait_for_version(dat$data$key, server)
  r <- httr::GET(server$api_url(dat$data$path))
  expect_equal(httr::status_code(r), 200)
  st <- content(r)
  expect_equal(st$status, "success")
  expect_is(st$data, "list")
  version <- st$data$version

  dest <- file.path(server$path, "archive", "count_param", version)
  wait_for_path(dest)
  wait_for_finished(dat$data$key, server)

  r <- httr::GET(server$api_url("/v1/report/info"),
                 query = list(name = "count_param",
                              id = version))
  expect_equal(httr::status_code(r), 200)
  info <- content(r)
  expect_equal(info$status, "success")
  expect_is(info$data, "list")

  expect_equal(info$data$id, version)
  expect_equal(info$data$name, "count_param")
  expect_equal(info$data$success, TRUE)
  expect_equal(info$data$git$branch, "master")
  expect_match(info$data$git$ref, "[0-9a-f]{7}")
  expect_null(info$data$error)
  expect_equal(info$data$params$time, 1)
  expect_equal(info$data$params$poll, 0.1)
})

test_that("can get missing dependencies of a workflow", {
  server <- start_test_server()
  on.exit(server$stop())

  sha <- git_ref_to_sha("HEAD", server$path)
  r <- httr::POST(server$api_url("/v1/workflow/summary/"),
                  body = list(reports = list(
                    list(
                      name = scalar("depend"),
                      instance = scalar("production")
                    ),
                    list(
                      name = scalar("count_param"),
                      params = list(
                        time = scalar(1)
                      )
                    )
                  ),
                  ref = scalar(sha)),
                  encode = "json")

  expect_equal(httr::status_code(r), 200)
  dat <- content(r)
  expect_equal(dat$status, "success")
  expect_equal(dat$data, list(
    reports = list(
      list(name = "depend",
           instance = "production"),
      list(
        name = "count_param",
        params = list(
          time = 1
        )
      )
    ),
    ref = sha,
    missing_dependencies = list(
      depend = "example",
      count_param = list()
    )
  ))
})

test_that("workflow can be run", {
  path <- orderly_prepare_orderly_git_example()
  server <- start_test_server(path[["local"]])
  on.exit(server$stop())

  sha <- git_ref_to_sha("HEAD", server$path)
  r <- httr::POST(server$api_url("/v1/workflow/run/"),
                  body = list(reports = list(
                    list(
                      name = scalar("global")
                    ),
                    list(
                      name = scalar("minimal")
                    )
                  ),
                  ref = scalar(sha)),
                  encode = "json")

  expect_equal(httr::status_code(r), 200)
  dat <- content(r)
  expect_equal(dat$status, "success")
  expect_equal(names(dat$data), c("workflow_key", "reports"))
  expect_length(dat$data$reports, 2)

  wait_for_finished(dat$data$reports[[1]]$key, server)
  wait_for_finished(dat$data$reports[[2]]$key, server)

  report_1_status <- httr::GET(server$api_url(
    sprintf("/v1/reports/%s/status/", dat$data$reports[[1]]$key)),
    query = list(output = TRUE))
  expect_equal(httr::status_code(report_1_status), 200)
  status_1 <- content(report_1_status)
  expect_equal(status_1$status, "success")
  expect_equal(status_1$data$status, "success")
  expect_true(!is.null(status_1$data$version))

  report_2_status <- httr::GET(server$api_url(
    sprintf("/v1/reports/%s/status/", dat$data$reports[[2]]$key)),
    query = list(output = TRUE))
  expect_equal(httr::status_code(report_2_status), 200)
  status_2 <- content(report_2_status)
  expect_equal(status_2$status, "success")
  expect_equal(status_2$data$status, "success")
  expect_true(!is.null(status_2$data$version))

  ## Can get workflow status
  workflow_status <- httr::GET(server$api_url(
    sprintf("/v1/workflow/%s/status/", dat$data$workflow_key)),
    query = list(output = TRUE))
  expect_equal(httr::status_code(workflow_status), 200)
  status <- content(workflow_status)
  expect_equal(status$status, "success")
  expect_equal(status$data$status, "success")
  expect_equal(status$data$workflow_key, dat$data$workflow_key)
  expect_equal(length(status$data$reports), 2)
  expect_setequal(status$data$reports, list(status_1$data, status_2$data))
})


test_that("Can cancel a running report", {
  server <- start_test_server()

  r <- httr::POST(server$api_url("/v1/reports/interactive/run/"),
                  body = NULL, encode = "json")
  dat <- content(r)
  key <- dat$data$key

  wait_for_version(key, server)

  r <- httr::DELETE(
    server$api_url("/v1/reports/%s/kill/", dat$data$key))
  httr::content(r)

  r <- httr::GET(server$api_url("/v1/reports/%s/status/", key))
  dat <- httr::content(r)

  expect_equal(dat$data$status, "interrupted")
})
