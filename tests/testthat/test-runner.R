context("orderly_runner")

test_that("queue works as intended", {
  skip_if_no_redis()

  path <- orderly_git_example("demo")
  runner <- orderly_runner_$new(path, NULL, queue_id = NULL, workers = 1,
                                worker_timeout = 300)
  expect_equal(runner$queue$worker_len(), 1)

  worker_1 <- runner$queue$worker_list()[[1]]

  expect_equal(nrow(runner$queue$worker_log_tail(worker_1)), 1)
  expect_equal(runner$queue$worker_log_tail(worker_1, n = 3)[1, "command"],
               "ALIVE")
  expect_equal(runner$queue$worker_log_tail(worker_1, n = 3)[3, "message"],
               "TIMEOUT_SET")

  expect_length(runner$queue$task_list(), 0)

  ## jobs can be pushed to queue
  job_id <- runner$queue$enqueue_(quote({
    Sys.sleep(1)
    1 + 1
  }))
  expect_length(runner$queue$task_list(), 1)

  ## status can be retrieved
  ## sleep to ensure job has been picked up by runner otherwise
  ## will be pending
  Sys.sleep(0.1)
  status <- runner$queue$task_status(job_id)
  expect_equivalent(status, "RUNNING")

  ## After task has completed
  Sys.sleep(1)
  result <- runner$queue$task_wait(job_id)
  status <- runner$queue$task_status(job_id)
  expect_equivalent(status, "COMPLETE")

  ## Cleanup removes workers
  con <- runner$con
  worker_name <- runner$queue$keys$worker_name
  expect_equal(con$SCARD(worker_name), 1)

  rm(runner)
  gc()

  expect_equal(con$SCARD(worker_name), 0)
})


test_that("queue_id is generated if not supplied", {
  withr::with_envvar(
    c("ORDERLY_SERVER_QUEUE_ID" = NA),
    expect_match(orderly_queue_id(NULL), "^orderly.server:[[:xdigit:]]+$"))
  withr::with_envvar(
    c("ORDERLY_SERVER_QUEUE_ID" = "myqueue"),
    expect_equal(orderly_queue_id(NULL), "myqueue"))
})


test_that("queue_id is required for workers", {
  withr::with_envvar(
    c("ORDERLY_SERVER_QUEUE_ID" = NA),
    expect_error(orderly_queue_id(NULL, TRUE),
                 "Environment variable 'ORDERLY_SERVER_QUEUE_ID' is not set"))
})

test_that("queue_id is returned if supplied", {
  withr::with_envvar(
    c("ORDERLY_SERVER_QUEUE_ID" = NA),
    expect_equal(orderly_queue_id("myqueue", TRUE), "myqueue"))
})


test_that("test runner can start workers with timeout", {
  skip_if_no_redis()
  path <- orderly_git_example("demo")
  runner <- orderly_runner_$new(path, NULL, queue_id = NULL, workers = 2,
                                worker_timeout = 300)
  timeout <- runner$queue$message_send_and_wait("TIMEOUT_GET",
                                                runner$queue$worker_list())
  expect_length(timeout, 2)
  expect_equal(timeout[[1]][["timeout"]], 300.0)
  expect_equal(timeout[[2]][["timeout"]], 300.0)
})

test_that("queue starts up normally without a timeout", {
  skip_if_no_redis()
  path <- orderly_git_example("demo")
  runner <- orderly_runner_$new(path, NULL, queue_id = NULL, workers = 1)
  timeout <- runner$queue$message_send_and_wait("TIMEOUT_GET",
                                               runner$queue$worker_list(),
                                               progress = FALSE)
  expect_equal(timeout[[1]], c("timeout" = Inf, remaining = Inf))
})


test_that("runner can run a report", {
  ## Setup dir for testing
  skip_if_no_redis()
  path <- orderly_git_example("demo")
  dir_create(dirname(path_stderr(path, "ignore")))
  dir_create(dirname(path_id_file(path, "ignore")))

  out <- runner_run("key_id", "test_key", path, "minimal", parameters = NULL,
                    instance = NULL, ref = NULL, changelog = NULL)
  expect_equal(out$report_name, "minimal")
  expect_match(out$report_id, "^\\d{8}-\\d{6}-\\w{8}")

  ## Report has been added to archive
  report_dir <- file.path(path, "archive", "minimal", out$report_id)
  expect_true(file.exists(report_dir))

  ## Report id has been written to redis
  con <- redux::hiredis()
  expect_equal(con$HGET("key_id", "test_key"), out$report_id)

  ## Can read log file as it has been copied to destination
  expect_true(file.exists(file.path(report_dir, "orderly.log")))
})


test_that("runner can run a report with parameters", {
  ## Setup dir for testing
  skip_if_no_redis()
  path <- orderly_git_example("demo")
  dir_create(dirname(path_stderr(path, "ignore")))
  dir_create(dirname(path_id_file(path, "ignore")))

  out <- runner_run("key_id", "test_key", path, "other",
                    parameters = list(nmin = 0.5), instance = NULL, ref = NULL,
                    changelog = NULL)
  expect_equal(out$report_name, "other")
  expect_match(out$report_id, "^\\d{8}-\\d{6}-\\w{8}")

  ## Report has been added to archive
  report_dir <- file.path(path, "archive", "other", out$report_id)
  expect_true(file.exists(report_dir))

  ## Report id has been written to redis
  con <- redux::hiredis()
  expect_equal(con$HGET("key_id", "test_key"), out$report_id)

  ## Can read log file as it has been copied to destination
  expect_true(file.exists(file.path(report_dir, "orderly.log")))

  run <- readRDS(orderly_path_orderly_run_rds(
    file.path(path, "archive", "other", out$report_id)))
  expect_equal(run$meta$parameters, list(nmin = 0.5))
})


test_that("runner can return errors", {
  ## Setup dir for testing
  skip_if_no_redis()
  path <- orderly_git_example("minimal")
  writeLines("1 + 1", file.path(path, "src/example/script.R"))
  dir_create(dirname(path_stderr(path, "ignore")))
  dir_create(dirname(path_id_file(path, "ignore")))

  err <- expect_error(runner_run("key_report_id", "test_key", path, "example",
                    parameters = NULL, instance = NULL, ref = NULL,
                    changelog = NULL))

  ## Report ID still can be retrieved
  con <- redux::hiredis()
  report_id <- con$HGET("key_report_id", "test_key")
  expect_match(report_id, "^\\d{8}-\\d{6}-\\w{8}")

  ## Report has been added to drafts
  draft <- file.path(path, "draft", "example", report_id)
  expect_true(file.exists(draft))

  ## Can read log file as it has been copied to destination
  expect_true(file.exists(file.path(draft, "orderly.log")))

  ## Error from run matches formatted orderly log
  log <- readLines(file.path(draft, "orderly.log"))
  expect_equal(err$message, paste(log, collapse = "\n"))
})


test_that("run: success", {
  testthat::skip_on_cran()
  skip_on_windows()
  skip_if_no_redis()
  path <- orderly_git_example("demo")
  expect_false(file.exists(file.path(path, "orderly.sqlite")))
  runner <- orderly_runner(path)
  expect_true(file.exists(file.path(path, "orderly.sqlite")))

  ## Run a report slow enough to reliably report back a "running" status
  key <- runner$submit_task_report("slow1")
  testthat::try_again(5, {
    Sys.sleep(0.5)
    status <- runner$status(key)
    expect_setequal(names(status), c("key", "status", "version", "start_time",
                                     "output", "queue"))
    expect_equal(status$key, key)
    expect_equal(status$status, "running")
    expect_true(!is.null(status$version))
    expect_null(status$output)
    expect_equal(status$queue, list())
  })

  task_id <- get_task_id_key(runner, key)
  expect_match(task_id, "^[[:xdigit:]]{32}$")
  result <- runner$queue$task_wait(task_id)
  report_id <- result$report_id
  expect_match(result$report_id, "^\\d{8}-\\d{6}-\\w{8}$")
  expect_equal(result$report_id, status$version)
  status <- runner$status(key, output = TRUE)
  expect_equal(status$key, key)
  expect_equal(status$status, "success")
  expect_equal(status$version, report_id)
  expect_true(status$start_time < as.numeric(Sys.time()))
  expect_true(status$start_time > as.numeric(Sys.time()) - 10)
  expect_match(status$output, paste0("\\[ id +\\]  ", report_id),
               all = FALSE)
  expect_equal(status$queue, list())

  ## Report is in archive
  d <- orderly::orderly_list_archive(path)
  expect_equal(d$name, "slow1")
  expect_equal(d$id, report_id)
})


test_that("run: error", {
  testthat::skip_on_cran()
  skip_on_windows()
  skip_if_no_redis()

  ## Setup report which will error
  path <- orderly_git_example("minimal")
  writeLines("1 + 1", file.path(path, "src/example/script.R"))

  runner <- orderly_runner(path)
  key <- runner$submit_task_report("example")
  task_id <- get_task_id_key(runner, key)
  expect_match(task_id, "^[[:xdigit:]]{32}$")
  result <- runner$queue$task_wait(task_id)

  expect_s3_class(result, "rrq_task_error")
  expect_match(result$message,
               "Script did not produce expected artefacts: mygraph.png")
  status <- runner$status(key, output = TRUE)
  expect_equal(status$key, key)
  expect_equal(status$status, "error")
  expect_true(any(grepl(
    "Script did not produce expected artefacts: mygraph.png",
    status$output)))
  expect_equal(status$queue, list())
})


test_that("run report with parameters", {
  testthat::skip_on_cran()
  skip_on_windows()
  skip_if_no_redis()
  path <- orderly_git_example("demo")
  runner <- orderly_runner(path)
  key <- runner$submit_task_report("other", parameters = list(nmin = 0.5))
  task_id <- get_task_id_key(runner, key)
  result <- runner$queue$task_wait(task_id)

  d <- orderly::orderly_list_archive(path)
  expect_equal(d$name, "other")
  expect_equal(d$id, result$report_id)

  d <- readRDS(orderly_path_orderly_run_rds(
    file.path(path, "archive", "other", result$report_id)))
  expect_equal(d$meta$parameters, list(nmin = 0.5))
})

test_that("run in branch (local)", {
  testthat::skip_on_cran()
  skip_on_windows()
  skip_if_no_redis()
  path <- orderly_prepare_orderly_git_example()
  runner <- orderly_runner(path[["local"]])

  key <- runner$submit_task_report("other", parameters = list(nmin = 0),
                                       ref = "other")
  task_id <- get_task_id_key(runner, key)
  result <- runner$queue$task_wait(task_id)

  d <- orderly::orderly_list_archive(path[["local"]])
  expect_equal(nrow(d), 1L)
  expect_equal(d$name, "other")
  expect_equal(d$id, result$report_id)
})

test_that("prevent ref change", {
  testthat::skip_on_cran()
  skip_if_no_redis()
  path <- orderly_unzip_git_demo()

  cfg <- list(
    database = list(
      source = list(
        driver = "RSQLite::SQLite",
        args = list(
          dbname = "dbname: source.sqlite"))),
    remote = list(
      main = list(
        driver = "orderly::orderly_remote_path",
        primary = TRUE,
        default_branch_only = TRUE,
        args = list(root = path))))
  writeLines(yaml::as.yaml(cfg), file.path(path, "orderly_config.yml"))

  runner <- orderly_runner(path, "main")
  expect_error(runner$submit_task_report("other", ref = "other"),
               "Reference switching is disallowed in this runner")
})

test_that("status missing ID", {
  testthat::skip_on_cran()
  skip_on_windows()
  skip_if_no_redis()
  path <- orderly_git_example("demo")
  runner <- orderly_runner(path, workers = 0)
  status <- runner$status("missing_key")
  expect_equal(status, list(
    key = "missing_key",
    status = "missing",
    version = NULL,
    start_time = NULL,
    output = NULL,
    queue = list()
  ))
})

test_that("check_timeout kills timed out reports", {
  testthat::skip_on_cran()
  skip_on_windows()
  skip_if_no_redis()
  path <- orderly_git_example("demo")
  runner <- orderly_runner(path, workers = 2)

  key <- runner$submit_task_report("slow10", timeout = 0)
  wait_for_id(runner, key)
  msg <- capture_messages(killed <- runner$check_timeout())
  task <- get_task_id_key(runner, key)
  expect_equal(killed, task)
  expect_equal(msg, sprintf(
    "Successfully killed '%s', exceeded timeout of 0\n", task))
})

test_that("check_timeout doesn't kill reports with long timeout", {
  testthat::skip_on_cran()
  skip_on_windows()
  skip_if_no_redis()
  path <- orderly_git_example("demo")
  runner <- orderly_runner(path, workers = 1)

  key <- runner$submit_task_report("slow10", timeout = 20)
  id <- wait_for_id(runner, key)
  msg <- capture_messages(killed <- runner$check_timeout())
  expect_null(killed)
  expect_equal(msg, character(0))
})

test_that("check_timeout returns NULL if no reports being run", {
  testthat::skip_on_cran()
  skip_on_windows()
  skip_if_no_redis()
  path <- orderly_git_example("interactive", testing = TRUE)
  runner <- orderly_runner(path)
  msg <- capture_messages(killed <- runner$check_timeout())
  expect_null(killed)
  expect_equal(msg, character(0))
})

test_that("check_timeout prints message if fails to kill a report", {
  testthat::skip_on_cran()
  skip_on_windows()
  skip_if_no_redis()
  path <- orderly_git_example("interactive", testing = TRUE)
  runner <- orderly_runner(path)

  ## Here I want to test the case where in between getting the logs which
  ## say that task A can be killed the task then completing before we call
  ## cancel, meaning that the call to cancel the task will fail. This makes
  ## for some pretty messy setup.
  ## Setup mock queue with realistic worker log
  key1 <- runner$submit_task_report("interactive", timeout = 0)
  Sys.sleep(0.5) ## Sleep to wait for runner to pick up task
  logs <- runner$queue$worker_log_tail()

  ## Create a mock queue to return our mocked log data
  queue <- runner$queue
  mock_queue <- list(
    worker_log_tail = function() logs,
    task_cancel = function(task_id, delete) stop("Failed to cancel"),
    ## Needed for cleanup
    worker_stop = function(type) queue$worker_stop(type = "kill"),
    destroy = function(delete) queue$destroy(delete = TRUE)
  )
  runner$queue <- mock_queue

  msg <- capture_messages(killed <- runner$check_timeout())
  expect_null(killed)
  task_id <- get_task_id_key(runner, key1)
  expect_equal(msg,
               sprintf("Failed to kill '%s'\n  Failed to cancel\n", task_id))
})


test_that("kill - when running", {
  testthat::skip_on_cran()
  skip_if_no_redis()
  skip_on_windows()
  path <- orderly_git_example("interactive", testing = TRUE)
  runner <- orderly_runner(path)
  name <- "interactive"
  key <- runner$submit_task_report(name)

  id <- wait_for_id(runner, key)
  expect_equal(runner$status(key)$status, "running")
  expect_equal(runner$kill(key), list(killed = TRUE, message = NA_character_))
  expect_equal(runner$status(key)$status, "interrupted")
  ret <- runner$kill(key)
  expect_false(ret$killed)
  expect_match(ret$message, sprintf("Failed to kill '%s'", key))
})

test_that("kill - whist queued", {
  testthat::skip_on_cran()
  skip_if_no_redis()
  skip_on_windows()
  path <- orderly_git_example("interactive", testing = TRUE)
  runner <- orderly_runner(path)
  name <- "interactive"
  key <- runner$submit_task_report(name)
  id <- wait_for_id(runner, key)

  key2 <- runner$submit_task_report(name)
  expect_true(runner$kill(key)$killed)
})

test_that("kill - no process", {
  testthat::skip_on_cran()
  skip_if_no_redis()
  path <- orderly_git_example("interactive", testing = TRUE)
  runner <- orderly_runner(path)
  key <- "virtual_plant"
  ret <- runner$kill(key)
  expect_false(ret$killed)
  expect_equal(ret$message,
               "Failed to kill 'virtual_plant'\n   task doesn't exist")
})

test_that("prevent git changes", {
  testthat::skip_on_cran()
  skip_on_windows()
  skip_if_no_redis()
  path <- orderly_prepare_orderly_git_example()

  cfg <- list(
    database = list(
      source = list(
        driver = "RSQLite::SQLite",
        args = list(
          dbname = "dbname: source.sqlite"))),
    remote = list(
      main = list(
        driver = "orderly::orderly_remote_path",
        primary = TRUE,
        default_branch_only = TRUE,
        args = list(root = path[["origin"]])),
      other = list(
        driver = "orderly::orderly_remote_path",
        args = list(root = path[["origin"]]))))

  path_local <- path[["local"]]
  writeLines(yaml::as.yaml(cfg), file.path(path_local, "orderly_config.yml"))

  runner <- withr::with_envvar(
    c("ORDERLY_API_SERVER_IDENTITY" = "main"),
    orderly_runner(path_local))
  expect_false(runner$allow_ref)
  expect_error(
    runner$submit_task_report("example", ref = "origin/other",
                              parameters = list(nmin = 0)),
    "Reference switching is disallowed in this runner")

  runner <- withr::with_envvar(
    c("ORDERLY_API_SERVER_IDENTITY" = "other"),
    orderly_runner(path_local))
  expect_true(runner$allow_ref)
  r <- runner$submit_task_report("example", ref = "origin/other",
                                 parameters = list(nmin = 0))

  runner <- withr::with_envvar(
    c("ORDERLY_API_SERVER_IDENTITY" = NA_character_),
    orderly_runner(path_local))
  expect_true(runner$allow_ref)
  r <- runner$submit_task_report("example", ref = "origin/other",
                    parameters = list(nmin = 0))
})


test_that("runner can set instance", {
  testthat::skip_on_cran()
  skip_on_windows()
  skip_if_no_redis()

  path <- orderly_git_example("demo")
  config <- file.path(path, "orderly_config.yml")
  p <- yaml::read_yaml(config)
  p$database$source$instances <- list(
    default = list(
      dbname = "source.sqlite"
    ),
    alternative = list(
      dbname = "alternative.sqlite"
    )
  )
  yaml::write_yaml(p, config)

  file.copy(file.path(path, "source.sqlite"),
            file.path(path, "alternative.sqlite"))

  con <- orderly::orderly_db("source", root = path, instance = "alternative")
  DBI::dbExecute(con$source, "DELETE from thing where id > 10")
  DBI::dbDisconnect(con$source)

  runner <- orderly_runner(path)
  key <- runner$submit_task_report("minimal", instance = "alternative")
  task_id <- get_task_id_key(runner, key)

  result <- runner$queue$task_wait(task_id)
  report_id <- result$report_id
  expect_match(result$report_id, "^\\d{8}-\\d{6}-\\w{8}")
  status <- runner$status(key, output = TRUE)
  expect_equal(status$key, key)
  expect_equal(status$status, "success")
  expect_equal(status$version, report_id)
  expect_true(status$start_time < as.numeric(Sys.time()))
  expect_true(status$start_time > as.numeric(Sys.time()) - 10)
  ## Data in alternative db extracts only 10 rows
  expect_match(status$output, "\\[ data +\\]  source => dat: 10 x 2",
               all = FALSE)
  expect_equal(status$queue, list())


  key_default <- runner$submit_task_report("minimal")
  task_id_default <- get_task_id_key(runner, key_default)

  result_default <- runner$queue$task_wait(task_id_default)
  report_id_default <- result_default$report_id
  expect_match(result_default$report_id, "^\\d{8}-\\d{6}-\\w{8}")
  status_default <- runner$status(key_default, output = TRUE)
  expect_equal(status_default$key, key_default)
  expect_equal(status_default$status, "success")
  expect_equal(status_default$version, report_id_default)
  expect_true(status_default$start_time < as.numeric(Sys.time()))
  expect_true(status_default$start_time > as.numeric(Sys.time()) - 10)
  ## Data in default db extracts 20 rows
  expect_match(status_default$output, "\\[ data +\\]  source => dat: 20 x 2",
               all = FALSE)
  expect_equal(status_default$queue, list())
})

test_that("status: clears task_timeout from redis", {
  testthat::skip_on_cran()
  skip_on_windows()
  skip_if_no_redis()
  path <- orderly_git_example("demo")
  expect_false(file.exists(file.path(path, "orderly.sqlite")))
  runner <- orderly_runner(path)
  expect_true(file.exists(file.path(path, "orderly.sqlite")))

  ## Run a report slow enough to reliably report back a "running" status
  key <- runner$submit_task_report("slow1", timeout = 10)
  task_id <- runner$con$HGET(runner$keys$key_task_id, key)
  testthat::try_again(5, {
    Sys.sleep(0.5)
    status <- runner$status(key)
    expect_equal(status$key, key)
    expect_equal(status$status, "running")
    ## timeout stored in redis
    task_id <- get_task_id_key(runner, key)
    expect_equal(runner$con$HGET(runner$keys$task_timeout, task_id), "10")
  })

  result <- runner$queue$task_wait(task_id)
  status <- runner$status(key, output = TRUE)
  expect_equal(status$key, key)
  expect_equal(status$status, "success")
  ## timeout has been removed from redis
  expect_null(runner$con$HGET(runner$keys$task_timeout, task_id))
})

test_that("runner run passes git args to orderly CLI", {
  skip_if_no_redis()
  mock_processx <- mockery::mock(list(
    is_alive = function() FALSE,
    get_exit_status = function() 0L,
    wait = function() TRUE), cycle = TRUE)
  mockery::stub(runner_run, "processx::process$new", mock_processx)
  run <- runner_run("key_report_id", "key", ".", "test", NULL, NULL,
                    ref = NULL, changelog = NULL)
  mockery::expect_called(mock_processx, 1)
  args <- mockery::mock_args(mock_processx)[[1]][[2]]
  expect_equal(args, c("--root", ".", "run", "test", "--print-log",
                       "--id-file", "./runner/id/key.id_file",
                       "--pull"))

  mockery::stub(runner_run, "processx::process$new", mock_processx)
  run <- runner_run("key_report_id", "key", ".", "test", NULL, NULL,
                    ref = "123", changelog = NULL)
  mockery::expect_called(mock_processx, 2)
  args <- mockery::mock_args(mock_processx)[[2]][[2]]
  expect_equal(args, c("--root", ".", "run", "test", "--print-log",
                       "--id-file", "./runner/id/key.id_file",
                       "--fetch", "--ref", "123"))

  mockery::stub(runner_run, "processx::process$new", mock_processx)
  run <- runner_run("key_report_id", "key", ".", "test", NULL, NULL,
                    ref = NULL, changelog = NULL)
  mockery::expect_called(mock_processx, 3)
  args <- mockery::mock_args(mock_processx)[[3]][[2]]
  expect_equal(args, c("--root", ".", "run", "test", "--print-log",
                       "--id-file", "./runner/id/key.id_file", "--pull"))
})


test_that("runner run passes changelog to orderly CLI", {
  skip_if_no_redis()
  mock_processx <- mockery::mock(list(
    is_alive = function() FALSE,
    get_exit_status = function() 0L,
    wait = function() TRUE), cycle = TRUE)
  mockery::stub(runner_run, "processx::process$new", mock_processx)
  run <- runner_run("key_report_id", "key", ".", "test", NULL, NULL,
                    ref = NULL, changelog = "[tst] message")
  mockery::expect_called(mock_processx, 1)
  args <- mockery::mock_args(mock_processx)[[1]][[2]]
  expect_equal(args, c("--root", ".", "run", "test", "--print-log",
                       "--id-file", "./runner/id/key.id_file",
                       "--pull", "--message", "[tst] message"))
})


test_that("status: lists queued tasks", {
  testthat::skip_on_cran()
  skip_on_windows()
  skip_if_no_redis()
  path <- orderly_git_example("interactive", testing = TRUE)
  runner <- orderly_runner(path)

  key1 <- runner$submit_task_report("interactive")
  key2 <- runner$submit_task_report("count_params",
                                    parameters = list(timeout = 10, poll = 1),
                                    changelog = "[internal] changelog")
  key3 <- runner$submit_task_report("interactive", instance = "production",
                                    ref = "123")
  key4 <- runner$submit_task_report("interactive")
  testthat::try_again(5, {
    Sys.sleep(0.5)
    key1_status <- runner$status(key1)
    expect_true(!is.null(key1_status$version))
    key4_status <- runner$status(key4)
    expect_equal(key4_status$key, key4)
    expect_equal(key4_status$status, "queued")
  })
  ## Key1 is running, key 2, 3 & 4 are queued
  key1_status <- runner$status(key1)
  expect_equal(key1_status$status, "running")
  key2_status <- runner$status(key2)
  expect_equal(key2_status$status, "queued")
  expect_equal(key2_status$queue, list(
    list(
      key = key1,
      status = "running",
      version = key1_status$version,
      inputs = list(
        name = "interactive",
        params = NULL,
        ref = NULL,
        instance = NULL,
        changelog = NULL)
    )
  ))
  key3_status <- runner$status(key3)
  expect_equal(key3_status$status, "queued")
  expect_equal(key3_status$queue, list(
    list(
      key = key1,
      status = "running",
      version = key1_status$version,
      inputs = list(
        name = "interactive",
        params = NULL,
        ref = NULL,
        instance = NULL,
        changelog = NULL)
    ),
    list(
      key = key2,
      status = "queued",
      version = NULL,
      inputs = list(
        name = "count_params",
        params = list(timeout = 10, poll = 1),
        ref = NULL,
        instance = NULL,
        changelog = "[internal] changelog"))))
  expect_equal(key4_status$queue, list(
    list(
      key = key1,
      status = "running",
      version = key1_status$version,
      inputs = list(
        name = "interactive",
        params = NULL,
        ref = NULL,
        instance = NULL,
        changelog = NULL)
    ),
    list(
      key = key2,
      status = "queued",
      version = NULL,
      inputs = list(
        name = "count_params",
        params = list(timeout = 10, poll = 1),
        ref = NULL,
        instance = NULL,
        changelog = "[internal] changelog")
    ),
    list(
      key = key3,
      status = "queued",
      version = NULL,
      inputs = list(
      name = "interactive",
      params = NULL,
      ref = "123",
      instance = "production",
      changelog = NULL))))
})

test_that("orderly runner won't start if root not under version control", {
  path <- orderly_prepare_orderly_example("minimal")
  expect_error(orderly_runner(path),
               "Not starting server as orderly root is not version controlled")
})

test_that("run: changelog", {
  testthat::skip_on_cran()
  skip_on_windows()
  skip_if_no_redis()
  path <- orderly_git_example("demo")
  runner <- orderly_runner(path)

  ## Run a report slow enough to reliably report back a "running" status
  key <- runner$submit_task_report("minimal", changelog = "[internal] test")

  task_id <- get_task_id_key(runner, key)
  expect_match(task_id, "^[[:xdigit:]]{32}$")
  result <- runner$queue$task_wait(task_id)
  report_id <- result$report_id
  status <- runner$status(key, output = TRUE)
  expect_equal(status$key, key)
  expect_equal(status$status, "success")
  expect_equal(status$version, report_id)

  ## Report is in archive
  d <- orderly::orderly_list_archive(path)
  expect_equal(d$name, "minimal")
  expect_equal(d$id, report_id)

  d <- readRDS(orderly_path_orderly_run_rds(
    file.path(path, "archive", "minimal", report_id)))
  expect_true(!is.null(d$meta$changelog))
  expect_equal(d$meta$changelog$label, "internal")
  expect_equal(d$meta$changelog$value, "test")
})

test_that("queue_status", {
  testthat::skip_on_cran()
  skip_on_windows()
  skip_if_no_redis()
  path <- orderly_git_example("interactive", testing = TRUE)
  runner <- orderly_runner(path)

  key1 <- runner$submit_task_report("interactive")
  key2 <- runner$submit_task_report("interactive", ref = "1234",
                                    instance = "production")
  key3 <- runner$submit_task_report("count_param", parameters = list(
    time = 10, poll = 1
  ), changelog = "[internal] changelog message")
  ## Ensure all tasks have been added to queue
  testthat::try_again(5, {
    Sys.sleep(0.5)
    key3_status <- runner$status(key3)
    expect_equal(key3_status$key, key3)
    expect_equal(key3_status$status, "queued")
  })

  ## Key1 is running, key 2, 3 are queued
  id1 <- runner$status(key1)$version
  queue_status <- runner$queue_status()
  expect_length(queue_status$tasks, 3)
  expect_equal(queue_status$tasks[[1]], list(
    key = key1,
    status = "running",
    version = id1,
    inputs = list(
      name = "interactive",
      params = NULL,
      ref = NULL,
      instance = NULL,
      changelog = NULL
    )
  ))
  expect_equal(queue_status$tasks[[2]], list(
    key = key2,
    status = "queued",
    version = NULL,
    inputs = list(
      name = "interactive",
      params = NULL,
      ref = "1234",
      instance = "production",
      changelog = NULL
    )
  ))
  expect_equal(queue_status$tasks[[3]], list(
    key = key3,
    status = "queued",
    version = NULL,
    inputs = list(
      name = "count_param",
      params = list(time = 10, poll = 1),
      ref = NULL,
      instance = NULL,
      changelog = "[internal] changelog message"
    )
  ))
})


test_that("submit_task_report can queue items with dependencies", {
  testthat::skip_on_cran()
  skip_on_windows()
  skip_if_no_redis()
  path <- orderly_git_example("minimal")
  mock_submit <- mockery::mock("1", "2", "3", cycle = TRUE)
  mock_orderly_runner <- R6::R6Class(
    "mock_orderly_runner",
    inherit = orderly_runner_,
    public = list(
      submit = function(expr, depends_on) {
        mock_submit(expr, depends_on = depends_on)
      }
    ))
  runner <- mock_orderly_runner$new(path, identity = NULL, queue_id = NULL,
                                    workers = 0)

  key1 <- runner$submit_task_report("example")
  key2 <- runner$submit_task_report("example", depends_on = key1)
  key3 <- runner$submit_task_report("example", depends_on = c(key1, key2))

  args <- mockery::mock_args(mock_submit)
  expect_equal(args[[1]]$depends_on, NULL)
  expect_equal(args[[2]]$depends_on, stats::setNames("1", key1))
  expect_equal(args[[3]]$depends_on,
               stats::setNames(c("1", "2"), c(key1, key2)))
})


test_that("status translation", {
  ## These get translated, and always will
  expect_equal(rrq_to_orderly_status(rrq:::TASK_PENDING), "queued")
  expect_equal(rrq_to_orderly_status(rrq:::TASK_COMPLETE), "success")

  ## these two might be worth changing in future
  expect_equal(rrq_to_orderly_status(rrq:::TASK_CANCELLED), "interrupted")
  expect_equal(rrq_to_orderly_status(rrq:::TASK_DIED), "orphan")

  ## These are directly passed through
  expect_equal(rrq_to_orderly_status(rrq:::TASK_DEFERRED), "deferred")
  expect_equal(rrq_to_orderly_status(rrq:::TASK_ERROR), "error")
  expect_equal(rrq_to_orderly_status(rrq:::TASK_IMPOSSIBLE), "impossible")
  expect_equal(rrq_to_orderly_status(rrq:::TASK_MISSING), "missing")
  expect_equal(rrq_to_orderly_status(rrq:::TASK_RUNNING), "running")
  expect_equal(rrq_to_orderly_status(rrq:::TASK_TIMEOUT), "timeout")
})
