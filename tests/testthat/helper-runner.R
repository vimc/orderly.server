runner_start <- function(runner, name, ...) {
  key <- runner$queue(name, ...)
  runner$poll()
  id <- wait_for_id(runner, key)
  list(name = name, key = key, id = id)
}

runner_start_interactive <- function(runner) {
  dat <- runner_start(runner, "interactive")
  wait_for_path(file.path(runner$path, "draft", dat$name, dat$id, "started"))
  dat
}

wait_while_running <- function(runner, ...) {
  wait_while(function() runner$poll() == "running")
}

skip_if_no_redis <- function() {
  available <- redux::redis_available()
  if (!available) {
    testthat::skip("Skipping test as redis is not available")
  }
  invisible(available)
}

get_task_id_key <- function(runner, key) {
  task_id <- runner$con$HGET(runner$keys$key_task_id, key)
}
