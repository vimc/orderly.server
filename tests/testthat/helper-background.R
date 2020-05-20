start_test_server <- function(path = NULL, port = 8321, log = NULL) {
  path <- path %||% orderly::orderly_example("interactive")
  server <- orderly_server_background(path, port, log)
  server$start()
  server
}


orderly_server_background <- function(path, port = 8321, log = NULL) {
  R6_orderly_server_background$new(path, port, log)
}


R6_orderly_server_background <- R6::R6Class(
  "orderly_server_background",

  public = list(
    path = NULL,
    port = NULL,
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
      self$port <- port

      self$log <- log %||% tempfile()
    },

    finalize = function() {
      self$stop()
    },

    start = function() {
      if (!is.null(self$pid)) {
        stop("Server already set up")
      }
      if (!private$server_not_up()) {
        stop("Server already listening on port ", port)
      }

      Sys.setenv(R_TESTS = "")

      libs <- sprintf("c(%s)",
                      paste(sprintf('"%s"', .libPaths()), collapse = ", "))
      code <-
        c(sprintf('path <- "%s"', self$path),
          sprintf("port <- %d", self$port),
          sprintf(".libPaths(%s)", libs),
          'orderly.server::server(path, port, "127.0.0.1", poll_interrupt = 1)')
      path_server <- tempfile()
      writeLines(code, path_server)

      unlink(self$log)
      Rscript <- file.path(R.home("bin"), "Rscript")
      self$pid <- sys::exec_background(Rscript, path_server,
                                       std_out = self$log, std_err = self$log)

      message("waiting for server to become responsive")
      wait_while(private$server_not_up)
      message("...OK")
    },

    stop = function() {
      if (!is.null(self$pid)) {
        message("Stopping server")
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
