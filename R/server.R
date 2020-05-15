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

  wait_for_go_signal(path, go_signal)
  runner <- orderly::orderly_runner(path, allow_ref)
  api <- build_api(runner)
  api$run(host, port)

  message("Server exiting")
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
