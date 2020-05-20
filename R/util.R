is_directory <- function(x) {
  isTRUE(file.info(x, extra_cols = FALSE)$isdir)
}


`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}


wait_while <- function(continue, timeout = 10, poll = 0.02) {
  t_start <- Sys.time()
  t_quit <- t_start + timeout
  while (continue()) {
    Sys.sleep(poll)
    now <- Sys.time()
    if (now > t_quit) {
      stop("Timeout reached")
    }
  }
  now - t_start
}

scalar <- function(x) {
  jsonlite::unbox(x)
}
