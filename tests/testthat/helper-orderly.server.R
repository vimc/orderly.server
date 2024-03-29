Sys.setenv(PORCELAIN_VALIDATE = "true") # nolint

content <- function(r) {
  txt <- httr::content(r, "text", encoding = "UTF-8")
  jsonlite::fromJSON(txt, simplifyDataFrame = FALSE)
}


empty_named_list <- function() {
  structure(list(), names = character(0))
}


## Copied from orderly's helpers
wait_for_path <- function(path, ...) {
  wait_while(function() !file.exists(path), ...)
}


wait_for_process_termination <- function(process, ...) {
  wait_while(process$is_alive, ...)
}


wait_for_finished <- function(key, server, ...) {
  is_running <- function() {
    r <- httr::GET(server$api_url("/v1/reports/%s/status/", key))
    !(content(r)$data$status %in% c("success", "error", "interrupted"))
  }
  wait_while(is_running, ...)
}


wait_for_version <- function(key, server) {
  url <- server$api_url("/v1/reports/%s/status/", key)
  wait_while(function() {
    r <- httr::GET(url)
    version <- content(r)$data$version
    is.null(version)
  })
}

wait_for_id <- function(runner, key, ...) {
  e <- environment()
  e$st <- NULL
  continue <- function() {
    e$st <- runner$status(key)
    e$st$status == "queued" ||
      (e$st$status == "running" && is.null(e$st$version))
  }
  wait_while(continue)
  e$st$version
}

wait_for_finished_runner <- function(runner, key) {
  is_running <- function() {
    runner$poll()
    status <- runner$status(key)$status
    status %in% c("queued", "running")
  }
  wait_while(is_running)
}


expect_valid_json <- function(json, schema) {
  testthat::skip_if_not_installed("jsonvalidate")
  if (utils::packageVersion("jsonvalidate") <= "1.1.0") {
    valid <- jsonvalidate::json_validate(json, schema)
  } else {
    valid <- jsonvalidate::json_validate(json, schema, engine = "ajv")
  }
  testthat::expect_true(valid)
}


expect_valid_response <- function(json, errors = list(), status = 200) {
  expect_valid_json(json, "spec/Response.schema.json")
}


read_json <- function(path, ...) {
  jsonlite::fromJSON(paste(readLines(path), collapse = "\n"), ...)
}


## There is going to be some work here to keep these up-to-date:
mock_runner <- function(key = NULL, status = NULL,
                        config = NULL, root = NULL,
                        check_timeout = NULL,
                        queue_status = NULL,
                        submit_workflow = NULL,
                        workflow_status = NULL,
                        default_branch = "master",
                        allow_ref = TRUE) {
  list(
    submit_task_report = mockery::mock(key, cycle = TRUE),
    status = mockery::mock(status, cycle = TRUE),
    check_timeout = mockery::mock(check_timeout, cycle = TRUE),
    kill = mockery::mock(list(killed = TRUE, message = NA_character_),
                         cycle = TRUE),
    config = config,
    root = root,
    default_branch = default_branch,
    allow_ref = allow_ref,
    queue_status = mockery::mock(queue_status, cycle = TRUE),
    submit_workflow = mockery::mock(submit_workflow, cycle = TRUE),
    workflow_status = mockery::mock(workflow_status, cycle = TRUE)
  )
}


## This is an experiment to reduce duplication in the testing.  It
## does not work for endpoints that take any args as these come
## through in two places, nor does it work for endpoints that have
## *path* args because $request() does not work there.
expect_simple_endpoint_runs <- function(endpoint, data, status_code = 200,
                                        content_type = "application/json") {
  res_endpoint <- endpoint$run()
  res_api <- endpoint$request()

  testthat::expect_equal(res_endpoint$status_code, status_code)
  testthat::expect_equal(res_endpoint$data, data)

  testthat::expect_equal(res_api$status, status_code)
  testthat::expect_equal(res_api$headers[["Content-Type"]], content_type)
  expect_equal(res_api$body, as.character(res_endpoint$body))
}


## nolint start
orderly_prepare_orderly_git_example <- orderly:::prepare_orderly_git_example
orderly_prepare_orderly_example <- orderly:::prepare_orderly_example
orderly_unzip_git_demo <- orderly:::unzip_git_demo
orderly_path_db_backup <- orderly:::path_db_backup
orderly_path_orderly_run_rds <- orderly:::path_orderly_run_rds
orderly_git_checkout_branch <- orderly:::git_checkout_branch
## nolint end

orderly_run_with_wait <- function(root, wait = 1, ...) {
  Sys.sleep(wait)
  orderly::orderly_run(root = root, ...)
}

orderly_git_example <- function(name, path = tempfile(), testing = FALSE) {
  orderly_prepare_orderly_example(name, path, testing = testing, git = TRUE)
}

version_info <- function() {
  scalar(as.character(utils::packageVersion("orderly.server")))
}

skip_on_windows <- function() {
  testthat::skip_on_os("windows")
}

with_sqlite <- function(path, fun) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  fun(con)
}

## RSQLite prints "call dbDisconnect() when finished working with a connection
## warning" once using momoised function warning_once. This is causing a
## segfault on CI sometimes when running test. Suspect it's grabbing a
## reference to a part of a connection object that is not always being
## garbage collected at the right time. Manually trigger a warning before
## running tests to avoid this.
trigger_dbi_warning <- function() {
  oo <- options(warn = 0)
  on.exit(options(oo))
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  rm(con)
  suppressWarnings(gc())
}

trigger_dbi_warning()

append_lines <- function(text, filename) {
  prev <- readLines(filename)
  writeLines(c(prev, text), filename)
}

expect_no_error <- function(object, ...) {
  expect_error(object, regexp = NA, ...)
}
