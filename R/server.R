##' Run orderly server
##' @title Run orderly server
##'
##' @param path Path to serve
##'
##' @param port Port to serve on
##'
##' @param host Optional
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
##' @param timeout_rate_limit How frequently should the API check for timeouts
##' default 2 mins.
##'
##' @param backup_period How frequently should backup be run, if NULL backup
##' is skipped
##'
##' @param log_level The "lgr" log level to use
##'
##' @param identity Optional server identity
##'
##' @export
server <- function(path, port, host = "0.0.0.0",
                   go_signal = NULL, queue_id = NULL, workers = 1,
                   backup_period = 600, timeout_rate_limit = 2 * 60,
                   log_level = "info", identity = NULL) {
  message("Starting orderly server on port ", port)
  message("Orderly root: ", path)

  wait_for_go_signal(path, go_signal)
  runner <- orderly_runner(path, identity, queue_id = queue_id,
                           workers = workers)
  api <- build_api(runner, runner$root,
                   backup_period = backup_period,
                   rate_limit = timeout_rate_limit,
                   logger = porcelain::porcelain_logger(log_level))
  api$run(host, port)

  message("Server exiting")
}


wait_for_go_signal <- function(path, go_signal) {
  if (is.null(go_signal)) {
    return(invisible())
  }
  if (!grepl("^(/|[A-Z][a-z]:)", go_signal) && !is.null(path)) {
    go_signal <- file.path(path, go_signal)
  }
  message(sprintf("Waiting for go signal at '%s'", go_signal))
  timeout <- 600
  poll <- 1
  elapsed <- wait_while(function() !file.exists(go_signal), timeout, poll)
  message(sprintf("Recieved go signal after %s", format(elapsed, digits = 1)))
}
