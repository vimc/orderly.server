##' Run orderly server
##' @title Run orderly server
##'
##' @param path Path to serve
##'
##' @param port Port to serve on
##'
##' @param host Optional
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
##' @param queue_id ID of an existing queue to connect to, creates a new one
##'   if NULL
##'
##' @param workers Number of workers to spawn
##'
##' @export
##' @importFrom httpuv runServer
server <- function(path, port, host = "0.0.0.0", allow_ref = TRUE,
                   go_signal = NULL, queue_id = NULL, workers = 1) {
  message("Starting orderly server on port ", port)
  message("Orderly root: ", path)

  wait_for_go_signal(path, go_signal)
  runner <- orderly_runner(path, allow_ref, queue_id = queue_id,
                           workers = workers)
  api <- build_api(runner, runner$root)
  api$run(host, port)

  message("Server exiting")
}


wait_for_go_signal <- function(path, go_signal) {
  if (is.null(go_signal)) {
    return(invisible())
  }
  if (!grepl("^(/|[A-Z][a-z]:)", go_signal)) {
    go_signal <- file.path(path, go_signal)
  }
  message(sprintf("Waiting for go signal at '%s'", go_signal))
  timeout <- 600
  poll <- 1
  elapsed <- wait_while(function() !file.exists(go_signal), timeout, poll)
  message(sprintf("Recieved go signal after %s", format(elapsed, digits = 1)))
}
