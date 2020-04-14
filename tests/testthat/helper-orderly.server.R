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
mock_runner <- function(keys = NULL, status = NULL) {
  list(
    queue = mockery::mock(keys, cycle = TRUE),
    status = mockery::mock(status, cycle = TRUE))
}
