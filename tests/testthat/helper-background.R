start_test_server <- function(path = NULL, port = 8321, log = NULL) {
  skip_if_no_redis()
  path <- path %||% orderly_git_example("interactive", testing = TRUE)
  get_free_port <- free_port(port)
  port <- get_free_port()
  server <- orderly_server_background$new(path, port, log)
  server$start()
  server
}

orderly_server_background <- R6::R6Class(
  "orderly_server_background",

  public = list(
    path = NULL,
    port = NULL,
    url = NULL,
    process = NULL,
    log = NULL,

    initialize = function(path, port, log) {
      loadNamespace("callr")
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
      if (!is.null(self$process)) {
        stop("Server already set up")
      }
      if (!private$server_not_up()) {
        stop("Server already listening on port ", port)
      }

      ## For testing don't rate limit server
      self$process <- callr::r_bg(
        function(path, port) {
          orderly.server::server(path, port, "127.0.0.1",
                                 timeout_rate_limit = 0)
        },
        args = list(path = self$path, port = self$port),
        stdout = self$log, stderr = self$log)

      message("waiting for server to become responsive")
      browser()
      wait_while(private$server_not_up)
      message("...OK")
    },

    stop = function() {
      if (!is.null(self$process) && self$process$is_alive()) {
        message("Stopping server")
        self$process$kill()
        self$process <- NULL
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


free_port <- function(start, max_tries = 20) {
  force(start)
  force(max_tries)
  function() {
    port <- find_free_port(start, max_tries)
    start <<- start + 1
    port
  }
}

find_free_port <- function(start, max_tries = 20) {
  port <- seq(start, length.out = max_tries)
  for (p in port) {
    if (check_port(p)) {
      return(p)
    }
  }
  stop(sprintf("Did not find a free port between %d..%d",
               min(port), max(port)),
       call. = FALSE)
}

check_port <- function(port) {
  timeout <- 0.1
  con <- tryCatch(suppressWarnings(socketConnection(
    "localhost", port = port, timeout = timeout, open = "r")),
    error = function(e) NULL)
  if (is.null(con)) {
    return(TRUE)
  }
  close(con)
  FALSE
}
