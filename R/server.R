##' Run orderly server
##' @title Run orderly server
##' @param path Path to serve
##' @param port Port to serve on
##' @param host Optional
##' @export
##' @importFrom httpuv runServer
##' @importFrom orderly orderly_runner
server <- function(path, port, host = "0.0.0.0") {
  message("Starting orderly server on port ", port)
  message("Orderly root: ", path)
  httpuv::runServer(host, port, server_app(path))
}

server_app <- function(path) {
  runner <- orderly::orderly_runner(path)
  map <- server_endpoints()
  list(call = function(req) server_handler(runner, req, map))
}

server_handler <- function(runner, req, map) {
  orderly::orderly_log(req$REQUEST_METHOD, req$PATH_INFO)

  catch <- function(e) {
    server_response(NA, e$message, 500)
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
       body = jsonlite::toJSON(body, auto_unbox = TRUE))
}

server_endpoints <- function() {
  runner <- orderly::orderly_runner(path)

  root <- function() {
    "orderly.server"
  }
  ## Wrapper functions to do a version -> id mapping
  status <- function(name, version, output = FALSE) {
    runner$status(name, version, output)
  }
  commit <- function(name, version) {
    runner$commit(name, version)
  }
  publish <- function(name, version, value = TRUE) {
    runner$publish(name, version, as_logical(value))
  }

  ## Order here matters because this will look through the first to
  ## the last, not anything clever with the least to most specific.
  ## For the endpoints listed here that's not a big deal.
  ##
  ## What is not dealt with here is any additional parameters; this is
  ## the case for run at least
  map <- list(list(dest = root,
                   path = "/",
                   query = NULL,
                   method = "GET"),
              list(dest   = runner$rebuild,
                   path   = "/reports/rebuild",
                   query  = NULL,
                   method = "POST"),
              list(dest   = runner$run,
                   path   = "/reports/:name/run",
                   query  = c("parameters", "commit"),
                   post   = "parameters",
                   method = "POST"),
              list(dest   = status,
                   path   = "/reports/:name/:version/status",
                   query  = "output",
                   method = "GET"),
              list(dest   = commit,
                   path   = "/reports/:name/:version/commit",
                   query  = NULL,
                   method = "POST"),
              list(dest   = publish,
                   path   = "/reports/:name/:version/publish",
                   query  = "value",
                   method = "POST"))
  lapply(map, function(x) c(x, parse_path(x$path)))
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

as_logical <- function(x, name = deparse(substitute(x))) {
  switch(tolower(x),
         "true" = TRUE,
         "yes" = TRUE,
         "false" = FALSE,
         "no" = FALSE,
         stop(sprintf("Invalid input for '%s'", name)))
}
