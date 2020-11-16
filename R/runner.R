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

runner_run <- function(key_report_id, key, root, name, parameters, instance,
                       ref, poll = 0.1) {
  con <- redux::hiredis()
  bin <- tempfile()
  dir.create(bin)
  orderly_bin <- write_script(
    bin,
    readLines(system.file("script", package = "orderly", mustWork = TRUE)),
    versioned = TRUE)
  id_file <- path_id_file(root, key)
  if (!is.null(parameters)) {
    parameters <- sprintf("%s=%s", names(parameters),
                          vcapply(parameters, format))
  }
  args <- c("--root", root,
            "run", name, "--print-log", "--id-file", id_file,
            if (!is.null(ref)) c("--ref", ref),
            if (!is.null(instance)) c("--instance", instance),
            parameters)
  log_err <- path_stderr(root, key)
  ## We don't need to capture stdout as orderly CLI interleaves stdout
  ## stderr for us
  px <- processx::process$new(orderly_bin, args,
                              stdout = NULL, stderr = log_err)
  ## Might be worth killing px on function exit
  id <- NA_character_
  while (px$is_alive()) {
    if (is.na(id)) {
      if (file.exists(id_file)) {
        id <- readlines_if_exists(id_file, NA_character_)
        con$HSET(key_report_id, key, id)
      }
    }
    Sys.sleep(poll)
  }

  ok <- px$get_exit_status() == 0L
  base <- if (ok) path_archive else path_draft
  p <- file.path(base(root), name, id)
  if (file.exists(p)) {
    file_copy(log_err, file.path(p, "orderly.log"))
  }

  if (!ok) {
    stop(paste(readLines(log_err), collapse = "\n"))
  }

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

    con = NULL,
    cleanup_on_exit = NULL,
    queue = NULL,
    queue_id = NULL,
    keys = NULL,

    ## Timeout is worker timeout
    initialize = function(root, allow_ref = NULL, backup_period, queue_id,
                          workers, cleanup_on_exit = workers > 0,
                          worker_timeout = Inf) {
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
      dir_create(dirname(path_id_file(self$root, "ignore")))

      ## Create queue
      self$cleanup_on_exit <- cleanup_on_exit
      message(sprintf("Connecting to redis at %s", redux::redis_config()$url))
      self$con <- redux::hiredis()
      message("Starting queue")
      self$queue_id <- orderly_queue_id(queue_id)
      self$queue <- rrq::rrq_controller(self$queue_id, self$con)
      self$queue$worker_config_save("localhost", heartbeat_period = 3)
      self$start_workers(workers, worker_timeout)
      self$keys <- orderly_key(self$queue$queue_id)
    },

    start_workers = function(workers, timeout) {
      if (workers > 0L) {
        ids <- rrq::worker_spawn(self$queue, workers)
        if (is.finite(timeout) && timeout > 0) {
          self$queue$message_send_and_wait("TIMEOUT_SET", timeout, ids)
        }
      }
    },

    submit_task_report = function(name, parameters = NULL, ref = NULL,
                                  instance = NULL, poll = 0.1, timeout = 600) {
      if (!self$allow_ref && !is.null(ref)) {
        stop("Reference switching is disallowed in this runner",
             call. = FALSE)
      }

      root <- self$root
      key <- ids::adjective_animal()
      ## TODO: key timeout? create timeout on queue, check jobs running, the time
      ## they have been running for and kill them if over timeout
      ## Add a check_timeout method as a hook on the api build which calls this
      ## SO then old jobs get killed
      self$con$HSET(self$keys$key_timeout, key, timeout)
      key_report_id <- self$keys$key_report_id
      task_id <- self$submit(quote(
        orderly.server:::runner_run(key_report_id, key, root, name,
                                    parameters, instance, ref, poll = poll)))
      self$con$HSET(self$keys$key_task_id, key, task_id)
      self$con$HSET(self$keys$task_id_key, task_id, key)
      key
    },

    submit = function(job, environment = parent.frame()) {
       self$queue$enqueue_(job, environment)
    },

    status = function(key, output = FALSE) {
      task_id <- self$con$HGET(self$keys$key_task_id, key)
      status <- unname(self$queue$task_status(task_id))
      out_status <- switch(status,
                           "PENDING" = "queued",
                           "COMPLETE" = "success",
                           tolower(status)
      )
      task_position <- self$queue$task_position(task_id)
      if (status %in% c("ERROR", "ORPHAN", "INTERRUPTED", "COMPLETE")) {
        task_position <- 0
      }
      report_id <- self$con$HGET(self$keys$key_report_id, key)
      if (output) {
        out <- list(
          readlines_if_exists(path_stderr(self$root, key), NULL)
        )
      } else {
        out <- NULL
      }

      list(
        key = key,
        status = out_status,
        version = report_id,
        output = out,
        task_position = task_position
      )
    },

    ## Not part of the api exposed functions, used in tests
    destroy = function() {
      self$queue$destroy(delete = TRUE)
    },

    cleanup = function() {
      if (self$cleanup_on_exit && !is.null(self$con)) {
        if (interactive()) {
          message("Stopping workers") # nocov
        }
        self$queue$worker_stop(type = "kill")
        self$destroy()
      }
    }
  ),

  private = list(
    finalize = function() {
      self$cleanup()
    }
  )
)


orderly_queue_id <- function(queue_id, worker = FALSE) {
  if (!is.null(queue_id)) {
    return(queue_id)
  }
  id <- Sys.getenv("ORDERLY_SERVER_QUEUE_ID", "")
  if (!nzchar(id)) {
    if (worker) {
      stop("Environment variable 'ORDERLY_SERVER_QUEUE_ID' is not set")
    }
    id <- sprintf("orderly.server:%s", ids::random_id())
  }
  id
}


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


path_id_file <- function(root, key) {
  file.path(root, "runner/id", paste0(key, ".id_file"))
}


orderly_key <- function(base) {
  list(key_task_id = sprintf("%s:orderly.server:key_task_id", base),
       task_id_key = sprintf("%s:orderly.server:task_id_key", base),
       key_timeout = sprintf("%s:orderly.server:key_timeout", base),
       key_report_id = sprintf("%s:orderly.server:key_report_id", base))
}
