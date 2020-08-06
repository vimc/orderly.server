Queue <- R6::R6Class(
  "Queue",
  cloneable = FALSE,
  public = list(
    root = NULL,
    cleanup_on_exit = NULL,
    queue = NULL,

    initialize = function(queue_id = NULL, workers = 1,
                          cleanup_on_exit = workers > 0) {
      self$cleanup_on_exit <- cleanup_on_exit

      message(sprintf("Connecting to redis at %s", redux::redis_config()$url))
      con <- redux::hiredis()

      message("Starting queue")
      queue_id <- orderly_queue_id(queue_id)
      self$queue <- rrq::rrq_controller(queue_id, con)
      self$queue$worker_config_save("localhost", heartbeat_period = 3)

      self$start(workers)
    },

    start = function(workers) {
      if (workers > 0L) {
        rrq::worker_spawn(self$queue, workers)
      }
    },

    submit = function(job) {
      self$queue$enqueue_(job)
    },

    status = function(key) {
      status <- unname(self$queue$task_status(key))
      out_status <- switch(status,
                           "PENDING" = "queued",
                           "COMPLETE" = "success",
                           tolower(status)
      )
      if (status %in% c("ERROR", "ORPHAN", "INTERRUPTED", "COMPLETE")) {
        list(status = out_status,
             version = "orderly job id",
             output = "stdout & stderr stuff",
             queue = 0)
      } else {
        list(status = out_status,
             version = "orderly job id",
             output = "stdout & stderr stuff",
             queue = self$queue$task_position(key))
      }
    },

    result = function(id) {
      self$queue$task_result(id)
    },

    cancel = function(id) {
      self$queue$task_cancel(id)
    },

    ## Not part of the api exposed functions, used in tests
    remove = function(id) {
      self$queue$task_delete(id)
    },

    ## Not part of the api exposed functions, used in tests
    destroy = function() {
      self$queue$destroy(delete = TRUE)
    },

    cleanup = function() {
      if (self$cleanup_on_exit && !is.null(self$queue$con)) {
        message("Stopping workers")
        self$queue$worker_stop()
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
