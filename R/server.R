##' Run orderly server
##' @title Run orderly server
##'
##' @param path Path to serve
##'
##' @param port Port to serve on
##'
##' @param host Optional
##'
##' @param poll_interrupt Interval (in ms) to poll for interrupt
##'
##' @param allow_ref Allow git reference changing (passed through to
##'   \code{orderly_runner}.
##'
##' @param go_signal If given, we poll for a file \code{go_signal}
##'   (within \code{path}) before starting.  This is designed
##'   primarily for use with docker where the data volume may not be
##'   ready at the same time as the process container (and indeed
##'   won't be if the container is used to provision the volume).
##'   During this period the server will not respond to any http
##'   requests.
##'
##' @export
##' @importFrom httpuv runServer
##' @importFrom orderly orderly_runner
server <- function(path, port, host = "0.0.0.0", poll_interrupt = NULL,
                   allow_ref = TRUE, go_signal = NULL) {
  message("Starting orderly server on port ", port)
  message("Orderly root: ", path)
  poll_interrupt <- poll_interrupt %||% 100

  app <- server_app(path, allow_ref, go_signal)
  server <- httpuv::startServer(host, port, app)
  on.exit(httpuv::stopServer(server))
  continue <- TRUE
  while (continue) {
    httpuv::service(poll_interrupt)
    tryCatch(app$poll(),
             error = function(e) NULL,
             interrupt = function(e) continue <<- FALSE)
    Sys.sleep(0.001)
  }
  message("Server exiting")
}

server_app <- function(path, allow_ref, go_signal) {
  wait_for_go_signal(path, go_signal)
  runner <- orderly::orderly_runner(path, allow_ref)
  map <- server_endpoints(runner)
  list(call = function(req) server_handler(req, map),
       poll = runner$poll)
}

wait_for_go_signal <- function(path, go_signal) {
  if (is.null(go_signal)) {
    return(invisible())
  }
  if (!grepl("^(/|[A-Z][a-z]:)", path)) {
    go_signal <- file.path(path, go_signal)
  }
  t0 <- Sys.time()
  while (!file.exists(go_signal)) {
    Sys.sleep(1)
    t <- Sys.time()
    dt <- time_diff_secs(t, t0)
    message(sprintf("[%s] waiting for go signal (%s) for %d s",
                    t, go_signal, dt))
  }
  message(sprintf("Recieved go signal after %d s",
                  time_diff_secs(Sys.time(), t0)))
}

server_handler <- function(req, map) {
  orderly::orderly_log(req$REQUEST_METHOD, req$PATH_INFO)

  catch <- function(e) {
    dat <- server_error_data("orderly-error",
                             paste("error while running orderly", e$message),
                             500)
    server_response(NA, dat$errors, dat$status)
  }

  dat <- parse_request(req, map)
  if (!is.null(dat$errors)) {
    ret <- server_response(NA, dat$errors, dat$status)
  } else {
    if (length(dat$args) > 0) {
      orderly::orderly_log(" `- args", paste(names(dat$args), collapse = ", "))
    }
    ret <- tryCatch(
      server_response(do.call(dat$dest, dat$args), list(), 200),
      error = catch)
  }

  orderly::orderly_log(" `->", ret$status)
  ret
}

server_response <- function(data, errors, status) {
  body <- list(status = if (status < 400) "success" else "failure",
               data = data,
               errors = errors)
  list(status = status,
       headers = list("Content-Type" = "application/json"),
       body = to_json(body))
}

server_endpoints <- function(runner) {
  ## TODO: get the error handling bits into here because otherwise
  ## we're all over the show with errors.
  index <- function() {
    list(name = "orderly.server",
         version = "0.0.0",
         endpoints = vapply(map, "[[", character(1), "path"))
  }
  ## NOTE: this ends up being exposed as a 'run' endpoint not a
  ## 'queue' endpoint because in the underlying runner queue/poll are
  ## separated but within the server both happen.
  run <- function(name, parameters = NULL, ref = NULL, update = TRUE,
                  timeout = NULL) {
    parameters <- as_orderly_parameter_list(parameters)
    key <- runner$queue(name, parameters, ref, as_logical(update),
                        timeout = as_integer(timeout, 600))
    list(name = name,
         key = key,
         path = sprintf("/v1/reports/%s/status/", key))
  }
  ## Wrapper functions to do a version -> id mapping
  status <- function(key, output = FALSE) {
    ret <- runner$status(key, as_logical(output))
    names(ret)[names(ret) == "id"] <- "version"
    if (is.null(ret$output)) {
      ret$output <- NA # maps to json 'null'
    }
    ret
  }
  kill <- function(key) {
    runner$kill(key)
  }
  publish <- function(name, version, value = TRUE) {
    orderly:::legacy_orderly_publish(
      name, version, as_logical(value), runner$config)
  }
  rebuild <- function() {
    runner$rebuild()
    NA
  }
  git_fetch <- function() {
    runner$git_fetch()$output
  }
  git_pull <- function() {
    runner$git_pull()$output
  }
  git_status <- function() {
    runner$git_status()[c("branch", "hash", "clean", "output")]
  }

  ## Order here matters because this will look through the first to
  ## the last, not anything clever with the least to most specific.
  ## For the endpoints listed here that's not a big deal.
  ##
  ## What is not dealt with here is any additional parameters; this is
  ## the case for run at least
  map <- list(list(name   = "index",
                   dest   = index,
                   path   = "/",
                   query  = NULL,
                   method = "GET"),
              list(name   = "rebuild",
                   dest   = rebuild,
                   path   = "/v1/reports/rebuild/",
                   query  = NULL,
                   method = "POST"),
              ## This does mean if we have a report called 'git' we
              ## won't be able to get it's status!
              list(name   = "git_status",
                   dest   = git_status,
                   path   = "/v1/reports/git/status/",
                   query  = NULL,
                   method = "GET"),
              list(name   = "git_fetch",
                   dest   = git_fetch,
                   path   = "/v1/reports/git/fetch/",
                   query  = NULL,
                   method = "POST"),
              list(name   = "git_pull",
                   dest   = git_pull,
                   path   = "/v1/reports/git/pull/",
                   query  = NULL,
                   method = "POST"),
              list(name   = "run",
                   dest   = run,
                   path   = "/v1/reports/:name/run/",
                   query  = c("parameters", "ref", "update", "timeout"),
                   post   = "parameters",
                   method = "POST"),
              list(name   = "status",
                   dest   = status,
                   path   = "/v1/reports/:key/status/",
                   query  = "output",
                   method = "GET"),
              list(name   = "kill",
                   dest   = kill,
                   path   = "/v1/reports/:key/kill/",
                   method = "DELETE"),
              list(name   = "publish",
                   dest   = publish,
                   path   = "/v1/reports/:name/:version/publish/",
                   query  = "value",
                   method = "POST"))
  res <- lapply(map, function(x) c(x, parse_path(x$path)))
  names(res) <- vapply(map, "[[", character(1), "name")
  res
}

server_match_endpoint <- function(path, map) {
  for (i in seq_along(map)) {
    if (grepl(map[[i]]$re, path)) {
      ret <- map[[i]]
      if (length(ret$args) > 0L) {
        v <- regexec(ret$re, path)[[1]]
        start <- v[-1L]
        end <- start + attr(v, "match.length")[-1L] - 1L
        args <- Map(substr, start, end, x = path)
        names(args) <- ret$args
        ret$args <- args
      } else {
        ret$args <- list()
      }
      return(ret)
    }
  }

  server_error_data("unknown-endpoint", "Endpoint not found", 404)
}

server_error_data <- function(code, description, status) {
  list(errors = list(list(code = code, message = description)),
       status = status)
}

parse_request <- function(req, map) {
  dat <- server_match_endpoint(req$PATH_INFO, map)
  if (!is.null(dat$errors)) {
    return(dat)
  }

  if (req$REQUEST_METHOD != dat$method) {
    msg <- paste("Request must use", dat$method)
    return(server_error_data("invalid-method", msg, 405))
  }

  query <- req$QUERY_STRING
  if (length(query) > 0 && nzchar(query)) {
    ## For now this will do; we can pull this out easily enough
    query <- webutils::parse_query(query)
    err <- setdiff(names(query), dat$query)
    if (length(err) > 0) {
      return(server_error_data("unknown-parameter", "Unknown query parameter",
                               405))
    }
    dat$args <- c(dat$args, query)
  }

  dat <- parse_request_post(req, dat)

  dat
}

parse_request_post <- function(req, dat) {
  has_post_data <- !is.null(req$CONTENT_LENGTH) && req$CONTENT_LENGTH > 0

  if (is.null(dat$post)) {
    if (has_post_data) {
      return(server_error_data("post-data-not-allowed",
                               "POST data not allowed for this endpoint",
                               400))
    }
  } else {
    if (has_post_data) {
      ## TODO: check is json?
      ## TODO: check json is valid
      dat$args[[dat$post]] <- rawToChar(req$rook.input$read())
    } else {
      dat$args[[dat$post]] <- NULL
    }
  }
  dat
}

parse_path <- function(x) {
  path <- x
  args <- NULL

  re_holder <- ":[a-zA-Z0-9]+"
  re_part <- "([^/]+)"

  repeat {
    v <- regexec(re_holder, x)[[1L]]
    if (v > 0) {
      args <- c(args, substr(x, v + 1L, v + attr(v, "match.length") - 1L))
      x <- sub(re_holder, re_part, x)
    } else {
      break
    }
  }

  list(re = sprintf("^%s$", x), args = args)
}
