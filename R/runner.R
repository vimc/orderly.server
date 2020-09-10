##' An orderly runner.  This is used to run reports as a server
##' process.  It's designed to be used in conjunction with OrderlyWeb,
##' so there is no "draft" stage and reports are committed as soon as
##' they are run.  This function is not intended for human end users,
##' only for creating automated tools for use with orderly.
##'
##' @title Orderly runner
##'
##' @param path Path to use
##'
##' @param allow_ref Allow git to change branches/ref for run.  If not
##'   given, then we will look to see if the orderly configuration
##'   disallows branch changes (based on the
##'   \code{ORDERLY_API_SERVER_IDENTITY} environment variable and the
##'   \code{master_only} setting of the relevant server block.
##'
##' @param backup_period Period (in seconds) between DB backups.  This
##'   is a guide only as backups cannot happen while a task is running
##'   - if more than this many seconds have elapsed when the runner is
##'   in its idle loop a backup of the db will be performed.  This
##'   creates a copy of orderly's destination database in
##'   \code{backup/db} with the same filename as the destination
##'   database, even if that database typically lives outside of the
##'   orderly tree.  In case of corruption of the database, this
##'   backup can be manually moved into place.  This is only needed if
##'   you are storing information alongside the core orderly tables
##'   (as done by OrderlyWeb).
##'
##' @export
##' @return A runner object, with methods designed for internal use only.
##' @examples
##'
##' path <- orderly::orderly_example("demo")
##' runner <- orderly.server::orderly_runner(path)
orderly_runner <- function(path, allow_ref = NULL, backup_period = 600,
                           queue_id = NULL, workers = 1) {
  orderly_runner_$new(path, allow_ref, backup_period, queue_id, workers)
}

orderly_runner_ <- R6::R6Class(
  "orderly_runner",
  cloneable = FALSE,
  public = list(
    path = NULL,
    config = NULL,
    allow_ref = FALSE,

    has_git = NULL,

    queue = NULL,

    initialize = function(path, allow_ref = NULL, backup_period, queue_id,
                          workers) {
      self$path <- path
      self$config <- orderly::orderly_config(path)
      self$has_git <- runner_has_git(path)
      if (!self$has_git) {
        message("Not enabling git features as this is not version controlled")
      }

      self$allow_ref <- runner_allow_ref(self$has_git, allow_ref, self$config)
      if (self$has_git && !self$allow_ref) {
        message("Disallowing reference switching in runner")
      }

      ## This ensures that the index will be present, which will be
      ## useful if something else wants to access the database!
      DBI::dbDisconnect(orderly::orderly_db("destination", self$config, FALSE))

      ## Create queue
      self$queue <- Queue$new(queue_id, workers)
    },

    submit_task_report = function(name, parameters = NULL, ref = NULL,
                                  instance = NULL, update = FALSE,
                                  timeout = 600) {
      if (!self$allow_ref && !is.null(ref)) {
        stop("Reference switching is disallowed in this runner",
             call. = FALSE)
      }

      self$queue$submit(quote(
        orderly:::orderly_run_internal(name, parameters, instance)
      ))
    },

    # submit_task_workflow = function(name, ref = NULL, instance = NULL,
    #                                 update = FALSE, timeout = 600) {
    #   if (!self$allow_ref && !is.null(ref)) {
    #     stop("Reference switching is disallowed in this runner",
    #          call. = FALSE)
    #   }
    #
    #   self$queue$submit(quote(
    #     orderly:::orderly_workflow_int(name, parameters, instance)
    #   ))
    # },

    status = function(key, output = FALSE) {
      self$queue$status(key)
    }
  )
)

runner_allow_ref <- function(has_git, allow_ref, config) {
  if (!has_git) {
    allow_ref <- FALSE
  }
  if (is.null(allow_ref)) {
    allow_ref <- !(config$server_options()$master_only %||% FALSE)
  }
  if (allow_ref) {
    res <- git_run(c("rev-parse", "HEAD"), root = config$root, check = FALSE)
    allow_ref <- res$success
  }
  allow_ref
}

runner_has_git <- function(path) {
  nzchar(Sys.which("git")) && file.exists(file.path(path, ".git"))
}

