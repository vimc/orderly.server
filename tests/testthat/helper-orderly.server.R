test_server <- function(port = 8123) {
  path <- orderly:::prepare_orderly_example("interactive")
  server(path, port, "127.0.0.1")
}

content <- function(r) {
  txt <- httr::content(r, "text", encoding = "UTF-8")
  jsonlite::fromJSON(txt, simplifyDataFrame = FALSE)
}

empty_named_list <- function() {
  structure(list(), names = character(0))
}

## Copied from orderly's helpers
wait_while <- function(continue, timeout = 1, poll = 0.02) {
  t_quit <- Sys.time() + timeout
  while (continue()) {
    Sys.sleep(poll)
    if (Sys.time() > t_quit) {
      stop("Timeout reached")
    }
  }
}
wait_for_path <- function(path, ...) {
  wait_while(function() !file.exists(path), ...)
}
wait_for_process_termination <- function(process, ...) {
  wait_while(function() process$is_alive(), ...)
}

api_url <- function(path, ...) {
  port <- readLines("orderly.server.port")
  paste0("http://localhost:", port, sprintf(path, ...))
}

cache <- new.env(parent = new.env())
Sys.setenv(R_TESTS = "")

start_test_server <- function(log = "orderly.server.log") {
  ## TODO: done right we'd inherit a lot of env vars from parent R
  ## (e.g., R_LIBS).  But this is done soon with new callr
  if (file.exists("orderly.server.path")) {
    file.remove("orderly.server.path")
  }
  Rscript <- file.path(R.home("bin"), "Rscript")
  px <- processx::process$new(Rscript, "orderly.server.R",
                              stdout = log, stderr = log)
  message("waiting for process to start")
  cache$px <- px
  wait_for_path("orderly.server.path")
  wait_for_path("orderly.server.port")

  url <- sprintf("http://localhost:%s/", readLines("orderly.server.port"))
  server_not_up <- function() {
    isTRUE(tryCatch(httr::GET(url), error = function(e) TRUE))
  }
  message("waiting for server to be responsive")
  wait_while(server_not_up)

  TRUE
}

stop_test_server <- function() {
  if (!is.null(cache$px)) {
    cache$px$kill(0)
    file.remove("orderly.server.path")
  }
}
