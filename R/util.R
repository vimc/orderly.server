is_directory <- function(x) {
  isTRUE(file.info(x, extra_cols = FALSE)$isdir)
}

as_logical <- function(x, name = deparse(substitute(x))) {
  switch(tolower(x),
         "true" = TRUE,
         "false" = FALSE,
         stop(sprintf("Invalid input for '%s'", name)))
}

to_json <- function(x, ...) {
  jsonlite::toJSON(x, auto_unbox = TRUE, ...)
}

time_diff_secs <- function(t, t0) {
  dt <- t - t0
  units(dt) <- "secs"
  as.integer(dt)
}


`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}


wait_while <- function(continue, timeout = 10, poll = 0.02) {
  t_quit <- Sys.time() + timeout
  while (continue()) {
    Sys.sleep(poll)
    if (Sys.time() > t_quit) {
      stop("Timeout reached")
    }
  }
}
