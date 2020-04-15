Sys.setenv(PKGAPI_VALIDATE = "true")

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
    !(content(r)$data$status %in% c("success", "error", "killed"))
  }
  wait_while(is_running, ...)
}


wait_for_id <- function(key, server) {
  url <- server$api_url("/v1/reports/%s/status/", key)
  wait_while(function() {
    r <- httr::GET(url)
    version <- content(r)$data$version
    is.null(version)
  })
}


wait_for_finished_runner <- function(runner, key) {
  is_running <- function() {
    runner$poll()
    status <- runner$status(key)$status
    status %in% c("queued", "running")
  }
  wait_while(is_running)
}


start_test_server <- function(path = NULL, port = 8321, log = NULL) {
  path <- path %||% orderly:::prepare_orderly_example("interactive")
  server <- orderly_server_background(path, port, log)
  server$start()
  server
}


test_runner <- function(path = tempfile()) {
  orderly:::prepare_orderly_example("interactive", path)
  server_endpoints(orderly::orderly_runner(path))
}


test_runner2 <- function(path = tempfile()) {
  orderly:::prepare_orderly_example("interactive", path)
  orderly::orderly_runner(path)
}


expect_valid_json <- function(json, schema) {
  testthat::skip_if_not_installed("jsonvalidate")
  if (packageVersion("jsonvalidate") <= "1.1.0") {
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
mock_runner <- function(keys = NULL, status = NULL, git_status = NULL,
                        git_fetch = NULL, git_pull = NULL) {
  list(
    rebuild = mockery::mock(TRUE, cycle = TRUE),
    queue = mockery::mock(keys, cycle = TRUE),
    status = mockery::mock(status, cycle = TRUE),
    kill = mockery::mock(TRUE, cycle = TRUE),
    git_status = mockery::mock(git_status, cycle = TRUE),
    git_fetch = mockery::mock(git_fetch, cycle = TRUE),
    git_pull = mockery::mock(git_pull, cycle = TRUE))
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
