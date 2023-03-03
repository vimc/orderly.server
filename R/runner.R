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
##' @param identity The name of the server identity, as listed in
##'   orderly_config.yml's remote section. If not given, then we will
##'   look to see if the orderly configuration disallows branch
##'   changes (based on the \code{ORDERLY_API_SERVER_IDENTITY}
##'   environment variable. Used to set configuration specific to this
##'   server (e.g., host, port, teams notification URL, default branch
##'   name, etc).
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
orderly_runner <- function(path, identity = NULL, queue_id = NULL,
                           workers = 1) {
  orderly_runner_$new(path, identity, queue_id, workers)
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

  check_for_id <- function() {
    id <- NA_character_
    if (file.exists(id_file)) {
      id <- readlines_if_exists(id_file, NA_character_)
      con$HSET(key_report_id, key, id)
    }
    id
  }
  id <- NA_character_
  ## Pull out ID and save in redis as soon as possible with a busy wait
  ## then block wait for processx to finish
  while (px$is_alive()) {
    id <- check_for_id()
    if (!is.na(id)) {
      break
    }
    Sys.sleep(poll)
  }
  px$wait()
  ## For very fast report running we can get into the state that
  ## id is still NA here. For example if in while loop
  ## 1. Check process is alive, try to read ID it doesn't exist yet so ID is NA
  ## 2. Whilst sleep is being run the report run finishes
  ## 3. On next check of px$is_alive it is false so the block is skipped and
  ##    ID never gets read out and so never gets set in redis
  ## so we check for ID again if it is still NA
  if (is.na(id)) {
    id <- check_for_id()
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
    #' @field identity Remote identity, as listed in orderly_config.yml
    identity = NULL,
    #' @field config Orderly config
    config = NULL,
    #' @field allow_ref Allow git to change branch/ref for run
    allow_ref = FALSE,
    #' @field default_branch Default git branch
    default_branch = NULL,

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
    #' @param identity Remote identity, as listed in orderly_config.yml
    #' @param queue_id ID of an existing queue to connect to, creates a new one
    #'   if NULL.
    #' @param workers Number of workers to spawn.
    #' @param cleanup_on_exit If TRUE workers are killed on exit.
    #' @param worker_timeout How long worker should live for before it is
    #' killed. Expect this is only finite during local testing.
    initialize = function(root, identity, queue_id,
                          workers, cleanup_on_exit = workers > 0,
                          worker_timeout = Inf) {
      if (is.null(identity)) {
        self$config <- orderly::orderly_config(root)
      } else {
        self$identity <- identity
        self$config <- withr::with_envvar(
          c(ORDERLY_API_SERVER_IDENTITY = identity),
          orderly::orderly_config(root))
      }
      self$root <- self$config$root
      if (!runner_has_git(self$root)) {
        stop("Not starting server as orderly root is not version controlled")
      }

      opts <- self$config$server_options()
      self$allow_ref <- !isTRUE(opts$default_branch_only)
      self$default_branch <- opts$default_branch %||% "master"
      message(sprintf("Default git branch: %s", self$default_branch))
      message(sprintf("%s reference switching in runner",
                      if (self$allow_ref) "Allowing" else "Disallowing"))

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
      self$queue <- rrq::rrq_controller$new(self$queue_id, self$con)
      self$queue$worker_config_save("localhost", heartbeat_period = 10)
      self$start_workers(workers, worker_timeout)
      self$keys <- orderly_key(self$queue$queue_id)
    },

    #' @description
    #' Re-read configuration and set options
    reload = function() {
      if (is.null(self$identity)) {
        self$config <- orderly::orderly_config(self$root)
      } else {
        self$config <- withr::with_envvar(
          c(ORDERLY_API_SERVER_IDENTITY = self$identity),
          orderly::orderly_config(self$root))
      }

      opts <- self$config$server_options()
      self$allow_ref <- !isTRUE(opts$default_branch_only)
      self$default_branch <- opts$default_branch %||% "master"
      message(sprintf("Default git branch: %s", self$default_branch))
      message(sprintf("%s reference switching in runner",
                      if (self$allow_ref) "Allowing" else "Disallowing"))
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
        ids <- rrq::rrq_worker_spawn(self$queue, workers)
        if (is.finite(timeout) && timeout > 0) {
          self$queue$message_send_and_wait("TIMEOUT_SET", timeout, ids)
        }
      }
      invisible(TRUE)
    },

    #' @description
    #' Check if ref switching is allowed in this runner. Errors if
    #' ref is non NULL and ref switching disallowed, otherwise
    #' does nothing.
    #'
    #' @param ref Input ref to check
    #'
    #' @return TRUE, called for side effects.
    assert_ref_switching_allowed = function(ref) {
      if (!self$allow_ref && !is.null(ref)) {
        stop("Reference switching is disallowed in this runner",
             call. = FALSE)
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
    #' @param depends_on Keys of any tasks which this report depends on
    #'
    #' @return The key for the job, note this is not the task id. The task id
    #' can be retrieved from redis using the key.
    submit_task_report = function(name, parameters = NULL, ref = NULL,
                                  instance = NULL, changelog = NULL,
                                  poll = 0.1, timeout = 60 * 60 * 3,
                                  depends_on = NULL) {
      self$assert_ref_switching_allowed(ref)
      root <- self$root
      key <- ids::adjective_animal()
      key_report_id <- self$keys$key_report_id
      expr <- as.call(list(quote(orderly.server:::runner_run), # nolint
                           key_report_id = key_report_id,
                           key = key, root = root, name = name,
                           parameters = parameters, instance = instance,
                           ref = ref, changelog = changelog, poll = poll))
      depends_on_ids <- NULL
      if (!is.null(depends_on)) {
        depends_on_ids <- vcapply(depends_on, function(dependent_key) {
          self$con$HGET(self$keys$key_task_id, dependent_key)
        })
      }
      task_id <- self$submit(expr, depends_on = depends_on_ids)
      self$con$HSET(self$keys$key_task_id, key, task_id)
      self$con$HSET(self$keys$task_id_key, task_id, key)
      self$con$HSET(self$keys$task_timeout, task_id, timeout)
      key
    },

    #' @description
    #' Submit an arbitrary job on the queue
    #'
    #' @param expr A quoted R expression.
    #' @param depends_on Task ids for any dependencies of this job.
    #'
    #' @return Task id
    submit = function(expr, depends_on = NULL) {
      self$queue$enqueue_(expr, depends_on = depends_on,
                          separate_process = TRUE)
    },

    #' @description
    #' Queue a workflow.
    #'
    #' @param reports Details of reports to be run.
    #' @param ref The git sha to run the workflow.
    #' @param changelog Description of changes to the reports - applied to
    #' all reports.
    #' @param poll How frequently to poll for the report ID being available.
    #' Applied to each of the reports run in the workflow
    #' @param timeout Timeout for each of the reports run as part
    #' of the workflow default 3 hours.
    #'
    #' @return The key for the workflow and each individual report
    submit_workflow = function(reports, ref = NULL, changelog = NULL,
                               poll = 0.1, timeout = 60 * 60 * 3) {
      self$assert_ref_switching_allowed(ref)
      workflow <- build_workflow(self$root, reports, ref)
      ## Build a list of dependencies
      ## report_name : [task_id1, task_id2, ...]
      ## There could be multiple tasks queued with the same name
      dependencies <- key_value_collector()
      queue_task <- function(i) {
        report <- workflow[[i]]
        depends_on <- dependencies$get(report$depends_on)
        key <- self$submit_task_report(name = report$name,
                                       parameters = report$params,
                                       ref = ref,
                                       instance = report$instance,
                                       changelog = changelog,
                                       poll = poll,
                                       timeout = timeout,
                                       depends_on = depends_on)
        dependencies$add(report$name, key)
        report$key <- key
        report$execution_order <- i
        report
      }
      reports <- lapply(seq_along(workflow), queue_task)
      report_keys <- vcapply(reports, "[[", "key")
      workflow_key <- ids::adjective_animal()
      redis_key <- workflow_redis_key(self$queue$queue_id, workflow_key)
      self$con$SADD(self$keys$key_workflows, workflow_key)
      self$con$SADD(redis_key, report_keys)
      list(
        workflow_key = workflow_key,
        reports = reports
      )
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
          start_time = NULL,
          output = NULL,
          queue = list()
        ))
      }
      out_status <- private$task_status(task_id)
      if (out_status == "queued") {
        queued <- self$queue_status(key)$tasks
      } else {
        queued <- list()
      }
      report_id <- self$con$HGET(self$keys$key_report_id, key)
      times <- round(self$queue$task_times(task_id))
      start_time <- times[1, "start"] # Only ever 1 task_id here so always 1 row
      if (is.na(start_time)) {
        start_time <- NULL
      }
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
        start_time = start_time,
        output = out,
        queue = queued
      )
    },

    #' @description
    #' Get the status of a workflow.
    #'
    #' @param workflow_key The workflow key.
    #' @param output If TRUE include the output from each job in the workflow.
    #'
    #' @return List containing the workflow_key, status and status of each
    #' job in the workflow.
    workflow_status = function(workflow_key, output = FALSE) {
      redis_key <- workflow_redis_key(self$queue$queue_id, workflow_key)
      report_keys <- self$con$SMEMBERS(redis_key)
      reports <- lapply(report_keys, self$status, output)
      report_status <- lapply(reports, "[[", "status")
      workflow_status <- workflow_combine_status(report_status)
      list(
        workflow_key = workflow_key,
        status = workflow_status,
        reports = reports
      )
    },

    #' @description
    #' Get the running and queued tasks in front of key in the queue.
    #'
    #' If \code{key} is NULL then includes all queued tasks.
    #'
    #' @param key The job key, if NULL returns all queued tasks.
    #'
    #' @return List containing the key, status and report name of any
    #' running tasks and any queued tasks in front of key in the queue.
    queue_status = function(key = NULL) {
      running_tasks <- self$queue$worker_task_id()
      if (is.null(key)) {
        queued_tasks <- self$queue$queue_list()
      } else {
        task_id <- self$con$HGET(self$keys$key_task_id, key)
        queued_tasks <- self$queue$task_preceeding(task_id)
      }
      tasks <- c(running_tasks, queued_tasks)
      list(
        tasks = lapply(unname(tasks), private$get_task_details)
      )
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
      ## TODO: this is *terrible* and will come out
      incomplete <- logs[logs$command == "REMOTE", ]

      if (nrow(incomplete) == 0) {
        return(invisible(NULL))
      }
      incomplete$timeout <- redux::from_redis_hash(
        self$con, self$keys$task_timeout, incomplete$message, f = as.numeric)
      now <- as.numeric(Sys.time())
      to_kill <- incomplete[incomplete$time + incomplete$timeout < now, ]

      kill_task <- function(task_id, timeout) {
        tryCatch({
          self$queue$task_cancel(task_id, delete = FALSE)
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
        return(list(
          killed = FALSE,
          message = sprintf("Failed to kill '%s'\n   task doesn't exist", key)
        ))
      }
      ret <- tryCatch({
        self$queue$task_cancel(task_id, delete = FALSE)
        list(
          killed = TRUE,
          message = NA_character_
        )
      }, error = function(e) {
        list(
          killed = FALSE,
          message = sprintf("Failed to kill '%s'\n  %s", key, e$message)
        )
      })
      ret
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
      rrq_to_orderly_status(
        unname(self$queue$task_status(task_id)))
    },

    get_task_details = function(task_id) {
      key <- self$con$HGET(self$keys$task_id_key, task_id)
      status <- private$task_status(task_id)
      report_id <- NULL
      if (identical(status, "running")) {
        report_id <- self$con$HGET(self$keys$key_report_id, key)
      }
      task_data <- self$queue$task_data(task_id)
      list(
        key = key,
        status = status,
        version = report_id,
        inputs = list(
          name = task_data$expr$name,
          params = task_data$expr$parameters,
          ref = task_data$expr$ref,
          instance = task_data$expr$instance,
          changelog = task_data$expr$changelog
        )
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


rrq_to_orderly_status <- function(status) {
  switch(status,
         "PENDING" = "queued",
         "COMPLETE" = "success",
         "CANCELLED" = "interrupted",
         "DIED" = "orphan",
         tolower(status))
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
       key_report_id = sprintf("%s:orderly.server:key_report_id", base),
       key_workflows = sprintf("%s:orderly.server:workflows", base))
}

workflow_redis_key <- function(base, workflow_key) {
  sprintf("%s:orderly.server:workflow:%s", base, workflow_key)
}
