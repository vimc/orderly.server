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
##'   path <- orderly:::prepare_orderly_git_example()
##'   runner <- orderly.server::orderly_runner(path[["local"]], workers = 0)
##' }
orderly_runner <- function(path, allow_ref = NULL, queue_id = NULL,
                           workers = 1) {
  orderly_runner_$new(path, allow_ref, queue_id, workers)
}

runner_run <- function(key_report_id, key, root, name, parameters, instance,
                       ref, changelog, poll = 0.1) {
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
  git_args <- NULL
  if (is.null(ref)) {
    git_args <- "--pull"
  } else {
    git_args <- c("--fetch", "--ref", ref)
  }
  msg_args <- NULL
  if (!is.null(changelog)) {
    msg_args <- c("--message", changelog)
  }
  args <- c("--root", root,
            "run", name, "--print-log", "--id-file", id_file,
            git_args, msg_args,
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

#' Object for managing running jobs on the redis queue
#'
#' @keywords internal
orderly_runner_ <- R6::R6Class(
  "orderly_runner",
  cloneable = FALSE,
  public = list(
    #' @field root Orderly root
    root = NULL,
    #' @field config Orderly config
    config = NULL,
    #' @field allow_ref Allow git to change branch/ref for run
    allow_ref = FALSE,

    #' @field con Redis connection
    con = NULL,
    #' @field cleanup_on_exit If TRUE workers are killed on exit
    cleanup_on_exit = NULL,
    #' @field queue The redis queue
    queue = NULL,
    #' @field queue_id Redis queue ID
    queue_id = NULL,
    #' @field keys Set of redis keys for mapping between key, report_id
    #' and task_id
    keys = NULL,

    #' @description
    #' Create object, read configuration and setup redis connection.
    #'
    #' @param root Orderly root.
    #' @param allow_ref Allow git to change branches/ref for run.  If not
    #'   given, then we will look to see if the orderly configuration
    #'   disallows branch changes (based on the
    #'   \code{ORDERLY_API_SERVER_IDENTITY} environment variable and the
    #'   \code{master_only} setting of the relevant server block.
    #' @param queue_id ID of an existing queue to connect to, creates a new one
    #'   if NULL.
    #' @param workers Number of workers to spawn.
    #' @param cleanup_on_exit If TRUE workers are killed on exit.
    #' @param worker_timeout How long worker should live for before it is
    #' killed. Expect this is only finite during local testing.
    initialize = function(root, allow_ref = NULL, queue_id,
                          workers, cleanup_on_exit = workers > 0,
                          worker_timeout = Inf) {
      self$config <- orderly::orderly_config(root)
      self$root <- self$config$root
      if (!runner_has_git(self$root)) {
        stop("Not starting server as orderly root is not version controlled")
      }

      self$allow_ref <- runner_allow_ref(allow_ref, self$config)
      if (!self$allow_ref) {
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

    #' @description
    #' Start n workers for this queue and optionally set a timeout.
    #'
    #' @param workers Number of workers to spawn.
    #' @param timeout How long worker should live for before it is
    #' killed. Expect this is only finite during local testing.
    #'
    #' @return TRUE, called for side effects.
    start_workers = function(workers, timeout) {
      if (workers > 0L) {
        ids <- rrq::worker_spawn(self$queue, workers)
        if (is.finite(timeout) && timeout > 0) {
          self$queue$message_send_and_wait("TIMEOUT_SET", timeout, ids)
        }
      }
      invisible(TRUE)
    },

    #' @description
    #' Queue a job to run an orderly report.
    #'
    #' @param name Name of report to be queued.
    #' @param parameters List of parameters to pass to report.
    #' @param ref The git sha to run the report.
    #' @param instance The db instance for the report to pull data from.
    #' @param changelog Description of changes to the report.
    #' @param poll How frequently to poll for the report ID being available.
    #' @param timeout Timeout for the report run default 3 hours.
    #'
    #' @return The key for the job, note this is not the task id. The task id
    #' can be retrieved from redis using the key.
    submit_task_report = function(name, parameters = NULL, ref = NULL,
                                  instance = NULL, changelog = NULL,
                                  poll = 0.1, timeout = 60 * 60 * 3) {
      if (!self$allow_ref && !is.null(ref)) {
        stop("Reference switching is disallowed in this runner",
             call. = FALSE)
      }

      root <- self$root
      key <- ids::adjective_animal()
      key_report_id <- self$keys$key_report_id
      task_id <- self$submit(quote(
        orderly.server:::runner_run(key_report_id, key, root, name,   # nolint
                                    parameters, instance, ref,
                                    changelog = changelog, poll = poll)))
      self$con$HSET(self$keys$key_task_id, key, task_id)
      self$con$HSET(self$keys$task_id_key, task_id, key)
      self$con$HSET(self$keys$task_timeout, task_id, timeout)
      key
    },

    #' @description
    #' Submit an arbitrary job on the queue
    #'
    #' @param job A quoted R expression.
    #' @param environment Environment to run the expression in.
    #'
    #' @return Task id
    submit = function(job, environment = parent.frame()) {
       self$queue$enqueue_(job, environment)
    },

    #' @description
    #' Get the status of a job
    #'
    #' @param key The job key.
    #' @param output If TRUE include the output from job running.
    #'
    #' @return List containing the key, status, report_id (if available),
    #' output and the position of the job in the queue.
    status = function(key, output = FALSE) {
      task_id <- self$con$HGET(self$keys$key_task_id, key)
      if (is.null(task_id)) {
        return(list(
          key = key,
          status = "missing",
          version = NULL,
          output = NULL,
          queue = list()
        ))
      }
      out_status <- private$task_status(task_id)
      if (out_status == "queued") {
        queued <- self$get_preceeding_tasks(key)
      } else {
        queued <- list()
      }
      report_id <- self$con$HGET(self$keys$key_report_id, key)
      if (output) {
        out <- readlines_if_exists(path_stderr(self$root, key), NULL)
      } else {
        out <- NULL
      }

      ## Clear up task_timeout key if task has completed i.e. if the task is
      ## not queued or running
      running_status <- c("queued", "running")
      if (!(out_status %in% running_status)) {
        self$con$HDEL(self$keys$task_timeout, task_id)
      }

      list(
        key = key,
        status = out_status,
        version = report_id,
        output = out,
        queue = queued
      )
    },

    #' @description
    #' Get details of jobs in the queue
    #'
    #' @return List containing the key, status and report name of any
    #' running tasks and any queued tasks in the queue.
    queue_status = function() {
      queued_tasks <- self$queue$task_list()
      ## task_list returns in order latest queued - first queued
      ## we want the reverse of this
      queued_tasks <- rev(queued_tasks)
      lapply(queued_tasks, private$get_task_details)
    },

    #' @description
    #' Get the running and queued tasks in front of key in the queue
    #'
    #' @param key The job key.
    #'
    #' @return List containing the key, status and report name of any
    #' running tasks and any queued tasks in front of key in the queue.
    get_preceeding_tasks = function(key) {
      task_id <- self$con$HGET(self$keys$key_task_id, key)
      running <- self$queue$worker_task_id()
      running_details <- lapply(unname(running), private$get_task_details)
      queued_tasks <- self$queue$task_preceeding(task_id)
      queued_details <- lapply(queued_tasks, private$get_task_details)
      c(running_details, queued_details)
    },

    #' @description
    #' Check if any running tasks have passed their timeouts. This is run
    #' by the API on a preroute - we check for timeouts everytime someone
    #' interacts with the API. Not intended to be run directly.
    #'
    #' @return List of killed reports.
    check_timeout = function() {
      logs <- self$queue$worker_log_tail()
      ## Incomplete tasks are those where latest log is a START message
      incomplete <- logs[logs$command == "TASK_START", ]
      if (nrow(incomplete) == 0) {
        return(invisible(NULL))
      }
      incomplete$timeout <- redux::from_redis_hash(
        self$con, self$keys$task_timeout, incomplete$message, f = as.numeric)
      now <- as.numeric(Sys.time())
      to_kill <- incomplete[incomplete$time + incomplete$timeout < now, ]

      kill_task <- function(task_id, timeout) {
        tryCatch({
          self$queue$task_cancel(task_id)
          message(sprintf("Successfully killed '%s', exceeded timeout of %s",
                          task_id, timeout))
          task_id
        }, error = function(e) {
          message(sprintf("Failed to kill '%s'\n  %s", task_id, e$message))
          NA_character_
        })
      }

      ## log message contains the task_id
      killed <- Map(kill_task, to_kill$message, to_kill$timeout)
      invisible(unname(unlist(killed[!is.na(killed)])))
    },

    #' @description
    #' Kill a job
    #'
    #' @param key The job key.
    kill = function(key) {
      task_id <- self$con$HGET(self$keys$key_task_id, key)
      if (is.null(task_id)) {
        porcelain::porcelain_stop(
          sprintf("Failed to kill '%s' task doesn't exist", key))
      }
      tryCatch(
        self$queue$task_cancel(task_id),
        error = function(e) {
          porcelain::porcelain_stop(
            sprintf("Failed to kill '%s'\n  %s", key, e$message))
        }
      )
      invisible(TRUE)
    },

    #' @description
    #' Destroy the queue. Not expected to be called directly, used in tests.
    destroy = function() {
      self$queue$destroy(delete = TRUE)
    },

    #' @description
    #' Cleanup workers and destroy the queue. Not expected to be called
    #' directly, gets registered as finaliser of the object.
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
    },

    task_status = function(task_id) {
      status <- unname(self$queue$task_status(task_id))
      switch(status,
             "PENDING" = "queued",
             "COMPLETE" = "success",
             tolower(status)
      )
    },

    get_task_details = function(task_id) {
      key <- self$con$HGET(self$keys$task_id_key, task_id)
      task_data <- self$queue$task_data(task_id)
      list(
        key = key,
        status = private$task_status(task_id),
        name = task_data$objects$name
      )
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


runner_allow_ref <- function(allow_ref, config) {
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
       task_timeout = sprintf("%s:orderly.server:task_timeout", base),
       key_report_id = sprintf("%s:orderly.server:key_report_id", base))
}
