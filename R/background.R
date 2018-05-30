##' Primarily for testing, start a server in the background
##' @title Start a background orderly.server
##' @param path Path to an orderly root
##' @param port Port to listen on
##' @param log Path to log to
##' @export
orderly_server_background <- function(path, port = 8321, log = NULL) {
  R6_orderly_server_background$new(path, port, log)
}


R6_orderly_server_background <- R6::R6Class(
  "orderly_server_background",

  public = list(
    path = NULL,
    url = NULL,
    pid = NULL,
    log = NULL,

    initialize = function(path, port, log) {
      loadNamespace("sys")
      loadNamespace("httr")

      if (!file.exists(file.path(path, "orderly_config.yml"))) {
        stop(sprintf("'%s' does not look like an orderly root", path))
      }
      self$path <- path

      self$url <- sprintf("http://localhost:%d/", port)
      if (!private$server_not_up()) {
        stop("Server already listening on port ", port)
      }

      libs <- sprintf("c(%s)",
                      paste(sprintf('"%s"', .libPaths()), collapse = ", "))
      code <-
        c(sprintf('path <- "%s"', path),
          sprintf("port <- %d", port),
          sprintf(".libPaths(%s)", libs),
          'orderly.server::server(path, port, "127.0.0.1", poll_interrupt = 1)')
      writeLines(code, file.path(path, "server.R"))

      self$log <- log %||% sprintf("%s/orderly.server.%d.log", path, port)
    },

    start = function() {
      if (!is.null(self$pid)) {
        stop("Server already set up")
      }
      if (!private$server_not_up()) {
        stop("Server already listening on port ", port)
      }

      Sys.setenv(R_TESTS = "")
      unlink(self$log)
      Rscript <- file.path(R.home("bin"), "Rscript")
      server <- file.path(self$path, "server.R")
      self$pid <- sys::exec_background(Rscript, server,
                                       std_out = self$log, std_err = self$log)

      message("waiting for server to become responsive")
      wait_while(private$server_not_up)
      message("...OK")
    },

    stop = function() {
      if (!is.null(self$pid)) {
        tools::pskill(self$pid, tools::SIGINT)
        Sys.sleep(0.15)
        tools::pskill(self$pid, tools::SIGKILL)
        self$pid <- NULL
      }
    },

    status = function() {
      if (private$server_not_up()) {
        "stopped"
      } else {
        "running"
      }
    },

    api_url = function(path, ...) {
      paste0("http://localhost:", self$port, sprintf(path, ...))
    }
  ),

  private = list(
    server_not_up = function() {
      isTRUE(tryCatch(httr::GET(self$url), error = function(e) TRUE))
    }
  ))
