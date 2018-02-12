test_server <- function(port = 8321) {
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
wait_while <- function(continue, timeout = 10, poll = 0.02) {
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
wait_for_finished <- function(key, server, ...) {
  is_running <- function() {
    r <- httr::GET(server$url("/v1/reports/%s/status/", key))
    !(content(r)$data$status %in% c("success", "error"))
  }
  wait_while(is_running, ...)
}
wait_for_id <- function(key, server) {
  url <- server$url("/v1/reports/%s/status/", key)
  wait_while(function() {
    r <- httr::GET(url)
    version <- content(r)$data$version
    is.null(version)
  })
}

make_api_url <- function(port) {
  force(port)
  function(path, ...) {
    paste0("http://localhost:", port, sprintf(path, ...))
  }
}

Sys.setenv(R_TESTS = "")

start_test_server <- function(path = NULL, port = 8321,
                              log = NULL,
                              fork = TRUE) {
  if (is.null(log)) {
    log <- sprintf("orderly.server.%d.log", port)
  }
  if (file.exists(log)) {
    file.remove(log)
  }

  url <- sprintf("http://localhost:%d/", port)

  server_not_up <- function() {
    isTRUE(tryCatch(httr::GET(url), error = function(e) TRUE))
  }
  if (!server_not_up()) {
    stop("Server already listening on port ", port)
  }

  if (is.null(path)) {
    path <- orderly:::prepare_orderly_example("interactive")
  }

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
    pid <- px$get_pid()
    cl <- NULL
  }

  message("waiting for server to be responsive")
  wait_while(server_not_up)

  list(path = path, port = port, pid = pid, log = log,
       fork = fork, cl = cl, px = px,
       url = make_api_url(port))
}

stop_test_server <- function(server) {
  if (server$fork) {
    tools::pskill(server$pid, tools::SIGINT)
    Sys.sleep(0.15)
    parallel::stopCluster(server$cl)
  } else {
    server$px$signal(tools::SIGINT)
    Sys.sleep(0.15)
    server$px$kill()
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

read_json <- function(path, ...) {
  jsonlite::fromJSON(paste(readLines(path), collapse = "\n"), ...)
}
