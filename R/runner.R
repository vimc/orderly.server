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
##' @param queue_id ID of an existing queue to connect to, creates a new one
##'   if NULL
##'
##' @param workers Number of workers to spawn
##'
##' @export
##' @return A runner object, with methods designed for internal use only.
##' @examples
##' available <- redux::redis_available()
##' if (available) {
##'   path <- orderly::orderly_example("demo")
##'   runner <- orderly.server::orderly_runner(path, workers = 0)
##' }
orderly_runner <- function(path, allow_ref = NULL, backup_period = 600,
                           queue_id = NULL, workers = 1) {
  orderly_runner_$new(path, allow_ref, backup_period, queue_id, workers)
}

runner_run <- function(key_id, key, root, name, parameters, instance, ref,
                       poll = 0.1) {
  con <- redux::hiredis()
  bin <- tempfile()
  dir.create(bin)
  orderly_bin <- write_script(
    bin,
    readLines(system.file("script", package = "orderly", mustWork = TRUE)),
    versioned = TRUE)
  id_file <- path_id_file(root, key)
  args <- c("--root", root,
            "run", name, "--print-log", "--id-file", id_file,
            if (!is.null(ref)) c("--ref", ref),
            if (!is.null(instance)) c("--instance", instance),
            parameters)
  log_out <- path_stdout(root, key)
  log_err <- path_stderr(root, key)
  px <- processx::process$new(orderly_bin, args,
                              stdout = log_out, stderr = log_err)
  ## Might be worth killing px on function exit
  id <- NULL
  while (px$is_alive()) {
    if (is.null(id)) {
      if (file.exists(id_file)) {
        id <- readlines_if_exists(id_file, NA_character_)
        con$HSET(key_id, key, id)
      }
    }
    Sys.sleep(poll)
  }

  ## TODO: Copy the log file into the final report location
  ## either draft or archive
  list(
    report_name = name,
    report_id = id
  )
}

orderly_runner_ <- R6::R6Class(
  "orderly_runner",
  cloneable = FALSE,
  public = list(
    root = NULL,
    config = NULL,
    allow_ref = FALSE,

    path_id = NULL,

    has_git = NULL,

    queue = NULL,
    con = NULL,
    keys = NULL,

    initialize = function(root, allow_ref = NULL, backup_period, queue_id,
                          workers) {
      self$config <- orderly::orderly_config(root)
      self$root <- self$config$root
      self$has_git <- runner_has_git(self$root)
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

      ## Create directories for storing logs and id files
      dir_create(dirname(path_stderr(self$root, "ignore")))
      dir_create(dirname(path_stdout(self$root, "ignore")))
      dir_create(dirname(path_id_file(self$root, "ignore")))

      ## Create queue
      self$queue <- Queue$new(queue_id, workers)
      self$con <- self$queue$queue$con
      ## TODO: put all the keys in this and read from here too
      self$keys <- orderly_key(self$queue$queue_id)
    },

    submit_task_report = function(name, parameters = NULL, ref = NULL,
                                  instance = NULL, poll = 0.1, timeout = 600) {
      if (!self$allow_ref && !is.null(ref)) {
        stop("Reference switching is disallowed in this runner",
             call. = FALSE)
      }

      root <- self$root
      key <- ids::adjective_animal()
      self$con$HSET("key_timeout", key, timeout)
      task_id <- self$queue$submit(quote(
        orderly.server:::runner_run("key_id", key, root, name, parameters,
                                    instance, ref, poll = poll)))
      self$con$HSET("key_task_id", key, task_id)
      self$con$HSET("task_id_key", task_id, key)
      key
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
      task_id <- self$con$HGET("key_task_id", key)
      status <- self$queue$status(task_id)
      report_id <- self$con$HGET("key_id", key)
      status$version <- report_id
      if (output) {
        status$output <- list(
          stderr = readlines_if_exists(path_stderr(self$root, key)),
          stdout = readlines_if_exists(path_stdout(self$root, key))
        )
      }

      # TODO: Handle errors
      if (status$status == "error") {
        res <- self$queue$result(task_id)
        status$output <- res$message
      }
      status
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

path_stderr <- function(root, key) {
  file.path(root, "runner/log", paste0(key, ".stderr"))
}

path_stdout <- function(root, key) {
  file.path(root, "runner/log", paste0(key, ".stdout"))
}

path_id_file <- function(root, key) {
  file.path(root, "runner/id", paste0(key, ".id_file"))
}


orderly_key <- function(base) {
  list(key_task_id = sprintf("%s:orderly.server:key_task_id", base),
       task_id_key = sprintf("%s:orderly.server:task_id_key", base))
}
