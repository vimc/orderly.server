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
  wait_while(process$is_alive, ...)
}
wait_for_finished <- function(id, ...) {
  is_running <- function() {
    r <- httr::GET(api_url("/v1/reports/example/%s/status/", id))
    identical(content(r)$data$status, "running")
  }
  wait_while(is_running, ...)
}

api_url <- function(path, ...) {
  paste0("http://localhost:", cache$server$port, sprintf(path, ...))
}

cache <- new.env(parent = new.env())
Sys.setenv(R_TESTS = "")

start_test_server <- function(log = "orderly.server.log", fork = TRUE) {
  if (file.exists(log)) {
    file.remove(log)
  }

  port <- 8123
  url <- sprintf("http://localhost:%d/", port)

  server_not_up <- function() {
    isTRUE(tryCatch(httr::GET(url), error = function(e) TRUE))
  }
  if (!server_not_up()) {
    stop("Server already listening on port ", port)
  }

  path <- orderly:::prepare_orderly_example("interactive")

  if (fork) {
    cl <- parallel::makeCluster(1L, "FORK", outfile = log)
    pid <- parallel::clusterEvalQ(cl, Sys.getpid())[[1]]
    parallel:::sendCall(cl[[1L]], "server", list(path, port, "127.0.0.1", 50))

    px <- NULL
  } else {
    writeLines(as.character(port), "orderly.server.port")
    writeLines(path, "orderly.server.path")
    writeLines(.libPaths(), "orderly.server.libs")
    Rscript <- file.path(R.home("bin"), "Rscript")
    px <- processx::process$new(Rscript, "orderly.server.R",
                                stdout = log, stderr = log)
    cl <- NULL
  }

  message("waiting for server to be responsive")
  wait_while(server_not_up)

  cache$server <- list(path = path, port = port, pid = pid, log = log,
                       fork = fork, cl = cl, px = px)

  TRUE
}

stop_test_server <- function() {
  if (!is.null(cache$server)) {
    if (cache$server$fork) {
      tools::pskill(cache$server$pid, tools::SIGINT)
      Sys.sleep(0.15)
      parallel::stopCluster(cache$server$cl)
    } else {
      cache$px$signal(tools::SIGINT)
      Sys.sleep(0.15)
      cache$px$kill()
    }
    rm(list = "server", envir = cache)
  }
}

expect_valid_json <- function(json, schema) {
  testthat::skip_if_not_installed("jsonvalidate")
  testthat::expect_true(jsonvalidate::json_validate(json, schema))
}

expect_valid_response <- function(json, errors = list(), status = 200) {
  expect_valid_json(json, "spec/Response.schema.json")
}

test_runner <- function(path = tempfile()) {
  orderly:::prepare_orderly_example("interactive", path)
  server_endpoints(orderly::orderly_runner(path))
}
