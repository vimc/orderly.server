context("orderly_runner")

test_that("queue works as intended", {
  skip_if_no_redis()

  path <- orderly_prepare_orderly_example("demo")
  runner <- orderly_runner_$new(path, NULL, backup_period = 600,
                                queue_id = NULL, workers = 1,
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
  job_id <- runner$submit(quote({
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
  path <- orderly_prepare_orderly_example("demo")
  runner <- orderly_runner_$new(path, NULL, backup_period = 600,
                                queue_id = NULL, workers = 2,
                                worker_timeout = 300)
  timeout <- runner$queue$message_send_and_wait("TIMEOUT_GET",
                                                runner$queue$worker_list())
  expect_length(timeout, 2)
  expect_equal(timeout[[1]][["timeout"]], 300.0)
  expect_equal(timeout[[2]][["timeout"]], 300.0)
})

test_that("queue starts up normally without a timeout", {
  skip_if_no_redis()
  path <- orderly_prepare_orderly_example("demo")
  runner <- orderly_runner_$new(path, NULL, backup_period = 600,
                                queue_id = NULL, workers = 1)
  timeout <- runner$queue$message_send_and_wait("TIMEOUT_GET",
                                               runner$queue$worker_list(),
                                               progress = FALSE)
  expect_equal(timeout[[1]], c("timeout" = Inf, remaining = Inf))
})


test_that("runner can run a report", {
  ## Setup dir for testing
  skip_if_no_redis()
  path <- orderly_prepare_orderly_example("demo")
  dir_create(dirname(path_stderr(path, "ignore")))
  dir_create(dirname(path_id_file(path, "ignore")))

  out <- runner_run("key_id", "test_key", path, "minimal", parameters = NULL,
                    instance = NULL, ref = NULL)
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
  path <- orderly_prepare_orderly_example("demo")
  dir_create(dirname(path_stderr(path, "ignore")))
  dir_create(dirname(path_id_file(path, "ignore")))

  out <- runner_run("key_id", "test_key", path, "other",
                    parameters = list(nmin = 0.5), instance = NULL, ref = NULL)
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
  path <- orderly_prepare_orderly_example("minimal")
  writeLines("1 + 1", file.path(path, "src/example/script.R"))
  dir_create(dirname(path_stderr(path, "ignore")))
  dir_create(dirname(path_id_file(path, "ignore")))

  err <- expect_error(runner_run("key_report_id", "test_key", path, "example",
                    parameters = NULL, instance = NULL, ref = NULL))

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
  skip_on_appveyor()
  skip_on_windows()
  skip_if_no_redis()
  path <- orderly_prepare_orderly_example("demo")
  expect_false(file.exists(file.path(path, "orderly.sqlite")))
  runner <- orderly_runner(path)
  expect_true(file.exists(file.path(path, "orderly.sqlite")))

  ## Run a report slow enough to reliably report back a "running" status
  key <- runner$submit_task_report("slow1")
  testthat::try_again(5, {
    Sys.sleep(1)
    status <- runner$status(key)
    expect_equal(names(status), c("key", "status", "version", "output",
                                  "task_position"))
    expect_equal(status$key, key)
    expect_equal(status$status, "running")
    expect_true(!is.null(status$version))
    expect_null(status$output)
    expect_equal(status$task_position, 0)
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
  expect_match(status$output, paste0("\\[ id +\\]  ", report_id),
               all = FALSE)
  expect_equal(status$task_position, 0)

  ## Report is in archive
  d <- orderly::orderly_list_archive(path)
  expect_equal(d$name, "slow1")
  expect_equal(d$id, report_id)
})


test_that("run: error", {
  testthat::skip_on_cran()
  skip_on_appveyor()
  skip_on_windows()
  skip_if_no_redis()

  ## Setup report which will error
  path <- orderly_prepare_orderly_example("minimal")
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
  expect_equal(status$task_position, 0)
})


test_that("run report with parameters", {
  testthat::skip_on_cran()
  skip_on_windows()
  skip_if_no_redis()
  path <- orderly_prepare_orderly_example("demo")
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
  skip_on_appveyor()
  skip_on_windows()
  skip_if_no_redis()
  path <- orderly_unzip_git_demo()
  runner <- orderly_runner(path)

  key <- runner$submit_task_report("other", parameters = list(nmin = 0),
                                       ref = "other")
  task_id <- get_task_id_key(runner, key)
  result <- runner$queue$task_wait(task_id)

  d <- orderly::orderly_list_archive(path)
  expect_equal(nrow(d), 1L)
  expect_equal(d$name, "other")
  expect_equal(d$id, result$report_id)
})

test_that("run missing ref", {
  testthat::skip_on_cran()
  skip_if_no_redis()
  path <- orderly_prepare_orderly_git_example()
  path1 <- path[["origin"]]
  path2 <- path[["local"]]

  sha1 <- git_ref_to_sha("HEAD", path1)
  sha2 <- git_ref_to_sha("HEAD", path2)

  runner <- orderly_runner(path2)

  expect_false(git_ref_exists("unknown", path2))

  key <- runner$submit_task_report("minimal", ref = "unknown")
  task_id <- get_task_id_key(runner, key)
  result <- runner$queue$task_wait(task_id)
  res <- runner$status(key)
  expect_equal(res$status, "error")
  expect_s3_class(result, "error")
  ## TODO: Improve error message when unknown ref for checking out
  expect_match(result$message, "Error code 128 running command:\n")

  ## TODO: sort update behaviour - should we just always pull?
  ## In orderly (on client) if --ref exists fetch first
  ## if it doesn't exist but if update does -- update then pull

  skip("fix update")
  id <- runner$queue("minimal", ref = sha1, update = TRUE)
  expect_is(id, "character")
  expect_equal(runner$data$length(), 1)

  expect_true(git_ref_exists(sha1, path2))
  expect_equal(git_ref_to_sha("HEAD", path2), sha2)

  id <- runner$queue("minimal", ref = NULL)
  expect_equal(git_ref_to_sha("HEAD", path2), sha2)

  id <- runner$queue("minimal", ref = NULL, update = TRUE)
  expect_equal(git_ref_to_sha("HEAD", path2), sha1)
})

test_that("prevent ref change", {
  testthat::skip_on_cran()
  skip_if_no_redis()
  path <- orderly_unzip_git_demo()
  runner <- orderly_runner(path, FALSE)
  expect_error(runner$submit_task_report("other", ref = "other"),
               "Reference switching is disallowed in this runner")
})

test_that("status missing ID", {
  testthat::skip_on_cran()
  skip_on_appveyor()
  skip_on_windows()
  skip_if_no_redis()
  path <- orderly_prepare_orderly_example("demo")
  runner <- orderly_runner(path, workers = 0)
  status <- runner$status("missing_key")
  expect_equal(status, list(
    key = "missing_key",
    status = "missing",
    version = NULL,
    output = NULL,
    task_position = 0
  ))
})

test_that("Can't git change", {
  testthat::skip_on_cran()
  skip_if_no_redis()
  path <- orderly_prepare_orderly_example("interactive", testing = TRUE)
  runner <- orderly_runner(path)
  expect_error(runner$submit_task_report("other", ref = "other"),
               "Reference switching is disallowed in this runner")
})

test_that("kill - when running", {
  testthat::skip_on_cran()
  skip_if_no_redis()
  skip_on_windows()
  path <- orderly_prepare_orderly_example("interactive", testing = TRUE)
  runner <- orderly_runner(path)
  name <- "interactive"
  key <- runner$submit_task_report(name)

  id <- wait_for_id(runner, key)
  expect_true(runner$kill(key))
  expect_error(runner$kill(key), sprintf("Failed to kill '%s'", key))
})

test_that("kill - whist queued", {
  testthat::skip_on_cran()
  skip_if_no_redis()
  skip_on_windows()
  path <- orderly_prepare_orderly_example("interactive", testing = TRUE)
  runner <- orderly_runner(path)
  name <- "interactive"
  key <- runner$submit_task_report(name)
  id <- wait_for_id(runner, key)

  key2 <- runner$submit_task_report(name)
  expect_true(runner$kill(key))
})

test_that("kill - no process", {
  testthat::skip_on_cran()
  skip_if_no_redis()
  path <- orderly_prepare_orderly_example("interactive", testing = TRUE)
  runner <- orderly_runner(path)
  key <- "virtual_plant"
  expect_error(runner$kill(key),
               "Failed to kill 'virtual_plant' task doesn't exist")
})

test_that("timeout", {
  skip("add timeout")
  testthat::skip_on_cran()
  skip_on_windows()
  skip_on_appveyor()
  path <- orderly_prepare_orderly_example("interactive", testing = TRUE)
  runner <- orderly_runner(path)
  name <- "interactive"
  key <- runner$queue(name, timeout = 0)
  runner$poll()
  id <- wait_for_id(runner, key)
  expect_equal(runner$poll(), structure("timeout", key = key))
  expect_equal(runner$poll(), "idle")
})

test_that("queue_status", {
  skip("return queue info in status")
  testthat::skip_on_cran()
  skip_on_windows()
  path <- orderly_prepare_orderly_example("interactive", testing = TRUE)
  runner <- orderly_runner(path)

  expect_equal(
    runner$queue_status(NULL),
    list(status = "idle", queue = runner_queue$new()$get_df(), current = NULL))

  name <- "interactive"
  key1 <- runner$queue(name)
  key2 <- runner$queue(name)

  expect_equal(
    runner$queue_status(NULL),
    list(status = "idle", queue = runner$data$get_df(), current = NULL))

  runner$poll()
  res <- runner$queue_status()
  expect_equal(res$status, "running")
  expect_equal(res$queue, runner$data$get_df())
  expect_equal(nrow(res$queue), 2)
  expect_equal(res$current$key, key1)
  expect_equal(res$current$name, "interactive")
  expect_is(res$current$start_at, "POSIXt")
  expect_is(res$current$kill_at, "POSIXt")

  expect_equal(res$current$elapsed + res$current$remaining, 600)
  expect_null(res$current$output)

  res <- runner$queue_status(TRUE)
  expect_is(res$current$output$stderr, "character")
  expect_is(res$current$output$stdout, "character")

  res <- runner$queue_status(limit = 1)
  expect_equal(nrow(res$queue), 1L)
})


test_that("queue status", {
  skip("return queue info in status")
  testthat::skip_on_cran()
  skip_on_windows()
  path <- orderly_prepare_orderly_example("interactive", testing = TRUE)
  runner <- orderly_runner(path)

  name <- "interactive"
  key1 <- runner$queue(name)
  key2 <- runner$queue(name)
  key3 <- runner$queue(name)

  expect_equal(runner$status(key1)$output$stdout, character())
  expect_equal(runner$status(key2)$output$stdout,
               sprintf("queued:%s:%s", key1, "interactive"))
  expect_equal(runner$status(key3)$output$stdout,
               sprintf("queued:%s:%s", c(key1, key2), "interactive"))

  tmp <- runner$poll()
  id <- wait_for_id(runner, key1)

  expect_null(runner$status(key1)$output$stdout)
  expect_equal(runner$status(key2)$output$stdout,
               sprintf("running:%s:%s", key1, "interactive"))
  expect_equal(runner$status(key3)$output$stdout,
               sprintf("%s:%s:%s", c("running", "queued"),
                       c(key1, key2), "interactive"))

  ## into the main bit of output here:
  expect_equal(names(runner$status(key1, output = TRUE)$output),
               c("stderr", "stdout"))

  writeLines("continue", file.path(path, "draft", name, id, "resume"))
  wait_while_running(runner)

  expect_null(runner$status(key1)$output$stdout)
  expect_equal(runner$status(key2)$output$stdout, character(0))
  expect_equal(runner$status(key3)$output$stdout,
               sprintf("queued:%s:%s", key2, "interactive"))
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
        master_only = TRUE,
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


test_that("allow ref logic", {
  testthat::skip_on_cran()
  path <- orderly_unzip_git_demo()
  config <- list(server_options = function() list(master_only = FALSE),
                 root = path)

  expect_false(runner_allow_ref(FALSE, TRUE, config))

  expect_false(runner_allow_ref(TRUE, FALSE, config))
  expect_true(runner_allow_ref(TRUE, TRUE, config))
  expect_true(runner_allow_ref(TRUE, NULL, config))

  config <- list(server_options = function() list(master_only = TRUE),
                 root = path)
  expect_false(runner_allow_ref(TRUE, FALSE, config))
  expect_true(runner_allow_ref(TRUE, TRUE, config))
  expect_false(runner_allow_ref(TRUE, NULL, config))

  config <- list(server_options = function() list(master_only = FALSE),
                 root = tempfile())
  expect_false(runner_allow_ref(TRUE, FALSE, config))
  expect_false(runner_allow_ref(TRUE, TRUE, config))
  expect_false(runner_allow_ref(TRUE, NULL, config))
})


test_that("backup", {
  skip("add backup back in on a hook")
  testthat::skip_on_cran()
  path <- orderly_prepare_orderly_example("minimal")
  id <- orderly::orderly_run("example", root = path, echo = FALSE)
  orderly::orderly_commit(id, root = path)

  db_orig <- file.path(path, "orderly.sqlite")
  dat_orig <- with_sqlite(db_orig, function(con)
    DBI::dbReadTable(con, "report_version"))

  runner <- orderly_runner(path, backup_period = 1)

  Sys.sleep(1.2)
  runner$poll()

  db_backup <- orderly_path_db_backup(path, "orderly.sqlite")
  expect_true(file.exists(db_backup))

  dat_backup <- with_sqlite(db_backup, function(con)
    DBI::dbReadTable(con, "report_version"))

  expect_equal(dat_orig, dat_backup)
})

test_that("runner can set instance", {
  testthat::skip_on_cran()
  skip_on_windows()
  skip_if_no_redis()

  path <- orderly_prepare_orderly_example("demo")
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
  ## Data in alternative db extracts only 10 rows
  expect_match(status$output, "\\[ data +\\]  source => dat: 10 x 2",
               all = FALSE)
  expect_equal(status$task_position, 0)


  key_default <- runner$submit_task_report("minimal")
  task_id_default <- get_task_id_key(runner, key_default)

  result_default <- runner$queue$task_wait(task_id_default)
  report_id_default <- result_default$report_id
  expect_match(result_default$report_id, "^\\d{8}-\\d{6}-\\w{8}")
  status_default <- runner$status(key_default, output = TRUE)
  expect_equal(status_default$key, key_default)
  expect_equal(status_default$status, "success")
  expect_equal(status_default$version, report_id_default)
  ## Data in default db extracts 20 rows
  expect_match(status_default$output, "\\[ data +\\]  source => dat: 20 x 2",
               all = FALSE)
  expect_equal(status_default$task_position, 0)
})
