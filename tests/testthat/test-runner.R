context("orderly_runner")

test_that("run: success", {
  testthat::skip_on_cran()
  skip_on_appveyor()
  skip_on_windows()
  path <- orderly_prepare_orderly_example("demo")
  expect_false(file.exists(file.path(path, "orderly.sqlite")))
  runner <- orderly_runner(path)
  expect_true(file.exists(file.path(path, "orderly.sqlite")))

  ## Run a report slow enough to reliably report back a "running" status
  task_id <- runner$submit_task_report("slow1")
  expect_equal(runner$status(task_id),
               list(
                 task_id = task_id,
                 status = "running",
                 queue = 0
               ))

  result <- runner$queue$queue$task_wait(task_id)
  report_id <- result$report_id
  expect_match(result$report_id, "^\\d{8}-\\d{6}-\\w{8}")
  status <- runner$status(task_id)
  expect_equal(status$task_id, task_id)
  expect_equal(status$status, "success")
  expect_equal(status$queue, 0)
  expect_equal(status$version, report_id)
  expect_match(status$output, paste0("\\[ id +\\]  ", report_id),
               all = FALSE)

  d <- orderly::orderly_list_archive(path)
  expect_equal(d$name, "slow1")
  expect_equal(d$id, report_id)
})

test_that("run: error", {
  testthat::skip_on_cran()
  skip_on_appveyor()
  skip_on_windows()

  ## Setup report which will error
  path <- orderly_prepare_orderly_example("minimal")
  writeLines("1 + 1", file.path(path, "src/example/script.R"))

  runner <- orderly_runner(path)
  task_id <- runner$submit_task_report("example")
  result <- runner$queue$queue$task_wait(task_id)

  expect_equal(result$message,
               "Script did not produce expected artefacts: mygraph.png")
  status <- runner$status(task_id)
  expect_equal(status$task_id, task_id)
  expect_equal(status$status, "error")
  expect_equal(status$queue, 0)
  ## TODO: report errors too
})


test_that("run report with parameters", {
  testthat::skip_on_cran()
  skip_on_windows()
  path <- orderly_prepare_orderly_example("demo")
  runner <- orderly_runner(path)
  task_id <- runner$submit_task_report("other", parameters = list(nmin = 0.5))

  result <- runner$queue$queue$task_wait(task_id)

  d <- orderly::orderly_list_archive(path)
  expect_equal(d$name, "other")
  expect_equal(d$id, result$report_id)

  d <- readRDS(orderly_path_orderly_run_rds(
    file.path(path, "archive", "other", result$report_id)))
  expect_equal(d$meta$parameters, list(nmin = 0.5))
})

## TODO: rebuild
# test_that("rebuild", {
#   testthat::skip_on_cran()
#   path <- orderly_prepare_orderly_example("minimal")
#   runner <- orderly_runner(path)
#
#   name <- "example"
#   id <- orderly::orderly_run(name, root = path, echo = FALSE)
#   orderly::orderly_commit(id, name, root = path)
#
#   path_db <- file.path(path, "orderly.sqlite")
#   file.remove(path_db)
#   expect_true(runner$rebuild())
#   expect_true(file.exists(path_db))
# })

test_that("run in branch (local)", {
  testthat::skip_on_cran()
  skip_on_appveyor()
  skip_on_windows()
  path <- orderly_unzip_git_demo()
  runner <- orderly_runner(path)

  task_id <- runner$submit_task_report("other", parameters = list(nmin = 0),
                                       ref = "other")
  result <- runner$queue$queue$task_wait(task_id)

  d <- orderly::orderly_list_archive(path)
  expect_equal(nrow(d), 1L)
  expect_equal(d$name, "other")
  expect_equal(d$id, result$report_id)
})

test_that("run missing ref", {
  testthat::skip_on_cran()
  path <- orderly_prepare_orderly_git_example()
  path1 <- path[["origin"]]
  path2 <- path[["local"]]

  sha1 <- git_ref_to_sha("HEAD", path1)
  sha2 <- git_ref_to_sha("HEAD", path2)

  runner <- orderly_runner(path2)

  expect_false(git_ref_exists("unknown", path2))

  task_id <- runner$submit_task_report("minimal", ref = "unknown")
  result <- runner$queue$queue$task_wait(task_id)
  res <- runner$status(task_id)
  expect_equal(res$status, "error")
  expect_s3_class(result, "error")
  ## TODO: Improve error message when unknown ref for checking out
  expect_match(result$message, "Error code 128 running command:\n")

  ## TODO: sort update behaviour - should we just always pull?
  # id <- runner$queue("minimal", ref = sha1, update = TRUE)
  # expect_is(id, "character")
  # expect_equal(runner$data$length(), 1)
  #
  # expect_true(git_ref_exists(sha1, path2))
  # expect_equal(git_ref_to_sha("HEAD", path2), sha2)
  #
  # id <- runner$queue("minimal", ref = NULL)
  # expect_equal(git_ref_to_sha("HEAD", path2), sha2)
  #
  # id <- runner$queue("minimal", ref = NULL, update = TRUE)
  # expect_equal(git_ref_to_sha("HEAD", path2), sha1)
})

test_that("prevent ref change", {
  testthat::skip_on_cran()
  path <- orderly_unzip_git_demo()
  runner <- orderly_runner(path, FALSE)
  expect_error(runner$submit_task_report("other", ref = "other"),
               "Reference switching is disallowed in this runner")
})

test_that("Can't git change", {
  testthat::skip_on_cran()
  path <- orderly_prepare_orderly_example("interactive", testing = TRUE)
  runner <- orderly_runner(path)
  expect_error(runner$submit_task_report("other", ref = "other"),
               "Reference switching is disallowed in this runner")
})

## TODO: cleanup
# test_that("cleanup", {
#   testthat::skip_on_cran()
#   path <- orderly_prepare_orderly_example("minimal")
#   on.exit(unlink(path, recursive = TRUE))
#
#   id <- orderly::orderly_run("example", root = path, echo = FALSE)
#   orderly::orderly_commit(id, root = path)
#
#   writeLines("1 + 1", file.path(path, "src/example/script.R"))
#   expect_error(orderly::orderly_run("example", root = path, echo = FALSE),
#                "Script did not produce")
#
#   runner <- orderly_runner(path)
#   expect_message(runner$cleanup(), "clean.+draft/example")
#   expect_silent(runner$cleanup())
#
#   expect_equal(
#     nrow(orderly::orderly_list_drafts(TRUE, root = path, include_failed = TRUE)),
#     0L)
#   expect_equal(orderly::orderly_list_archive(FALSE, root = path)$id, id)
# })


# test_that("kill", {
#   testthat::skip_on_cran()
#   skip_on_windows()
#   skip_on_appveyor()
#   ## TODO: This test is fails to kill the process on travis but adding
#   ## print lines for debugging makes it pass on travis
#   ## We should fix this at some point, see VIMC-4066
#   skip_on_travis()
#   path <- orderly_prepare_orderly_example("interactive", testing = TRUE)
#   runner <- orderly_runner(path)
#   name <- "interactive"
#   key <- runner$queue(name)
#   runner$poll()
#   id <- wait_for_id(runner, key)
#   expect_true(runner$kill(key))
#   expect_error(runner$kill(key), "Can't kill")
#   expect_null(runner$process)
#   expect_equal(runner$poll(), "idle")
# })
#
# test_that("kill - wrong process", {
#   testthat::skip_on_cran()
#   skip_on_windows()
#   skip_on_appveyor()
#   path <- orderly_prepare_orderly_example("interactive", testing = TRUE)
#   runner <- orderly_runner(path)
#   name <- "interactive"
#   key <- runner$queue(name)
#   runner$poll()
#   id <- wait_for_id(runner, key)
#
#   key2 <- "virtual_plant"
#   expect_error(runner$kill(key2),
#                sprintf("Can't kill '%s' - currently running '%s'", key2, key))
#   runner$kill(key)
# })
#
# test_that("kill - no process", {
#   testthat::skip_on_cran()
#   path <- orderly_prepare_orderly_example("interactive", testing = TRUE)
#   runner <- orderly_runner(path)
#   key <- "virtual_plant"
#   expect_error(runner$kill(key),
#                "Can't kill 'virtual_plant' - not currently running a report")
# })
#
# test_that("timeout", {
#   testthat::skip_on_cran()
#   skip_on_windows()
#   skip_on_appveyor()
#   path <- orderly_prepare_orderly_example("interactive", testing = TRUE)
#   runner <- orderly_runner(path)
#   name <- "interactive"
#   key <- runner$queue(name, timeout = 0)
#   runner$poll()
#   id <- wait_for_id(runner, key)
#   expect_equal(runner$poll(), structure("timeout", key = key))
#   expect_equal(runner$poll(), "idle")
# })

# test_that("queue_status", {
#   testthat::skip_on_cran()
#   skip_on_windows()
#   path <- orderly_prepare_orderly_example("interactive", testing = TRUE)
#   runner <- orderly_runner(path)
#
#   expect_equal(
#     runner$queue_status(NULL),
#     list(status = "idle", queue = runner_queue$new()$get_df(), current = NULL))
#
#   name <- "interactive"
#   key1 <- runner$queue(name)
#   key2 <- runner$queue(name)
#
#   expect_equal(
#     runner$queue_status(NULL),
#     list(status = "idle", queue = runner$data$get_df(), current = NULL))
#
#   runner$poll()
#   res <- runner$queue_status()
#   expect_equal(res$status, "running")
#   expect_equal(res$queue, runner$data$get_df())
#   expect_equal(nrow(res$queue), 2)
#   expect_equal(res$current$key, key1)
#   expect_equal(res$current$name, "interactive")
#   expect_is(res$current$start_at, "POSIXt")
#   expect_is(res$current$kill_at, "POSIXt")
#
#   expect_equal(res$current$elapsed + res$current$remaining, 600)
#   expect_null(res$current$output)
#
#   res <- runner$queue_status(TRUE)
#   expect_is(res$current$output$stderr, "character")
#   expect_is(res$current$output$stdout, "character")
#
#   res <- runner$queue_status(limit = 1)
#   expect_equal(nrow(res$queue), 1L)
# })
#
#
# test_that("queue status", {
#   testthat::skip_on_cran()
#   skip_on_windows()
#   path <- orderly_prepare_orderly_example("interactive", testing = TRUE)
#   runner <- orderly_runner(path)
#
#   name <- "interactive"
#   key1 <- runner$queue(name)
#   key2 <- runner$queue(name)
#   key3 <- runner$queue(name)
#
#   expect_equal(runner$status(key1)$output$stdout, character())
#   expect_equal(runner$status(key2)$output$stdout,
#                sprintf("queued:%s:%s", key1, "interactive"))
#   expect_equal(runner$status(key3)$output$stdout,
#                sprintf("queued:%s:%s", c(key1, key2), "interactive"))
#
#   tmp <- runner$poll()
#   id <- wait_for_id(runner, key1)
#
#   expect_null(runner$status(key1)$output$stdout)
#   expect_equal(runner$status(key2)$output$stdout,
#                sprintf("running:%s:%s", key1, "interactive"))
#   expect_equal(runner$status(key3)$output$stdout,
#                sprintf("%s:%s:%s", c("running", "queued"),
#                        c(key1, key2), "interactive"))
#
#   ## into the main bit of output here:
#   expect_equal(names(runner$status(key1, output = TRUE)$output),
#                c("stderr", "stdout"))
#
#   writeLines("continue", file.path(path, "draft", name, id, "resume"))
#   wait_while_running(runner)
#
#   expect_null(runner$status(key1)$output$stdout)
#   expect_equal(runner$status(key2)$output$stdout, character(0))
#   expect_equal(runner$status(key3)$output$stdout,
#                sprintf("queued:%s:%s", key2, "interactive"))
# })


test_that("prevent git changes", {
  testthat::skip_on_cran()
  skip_on_windows()
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
  expect_equal(nrow(runner$queue_status()$queue), 1L)

  runner <- withr::with_envvar(
    c("ORDERLY_API_SERVER_IDENTITY" = NA_character_),
    orderly_runner(path_local))
  expect_true(runner$allow_ref)
  r <- runner$submit_task_report("example", ref = "origin/other",
                    parameters = list(nmin = 0))
  expect_equal(nrow(runner$queue_status()$queue), 1L)
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


# test_that("backup", {
#   testthat::skip_on_cran()
#   path <- orderly_prepare_orderly_example("minimal")
#   id <- orderly::orderly_run("example", root = path, echo = FALSE)
#   orderly::orderly_commit(id, root = path)
#
#   db_orig <- file.path(path, "orderly.sqlite")
#   dat_orig <- with_sqlite(db_orig, function(con)
#     DBI::dbReadTable(con, "report_version"))
#
#   runner <- orderly_runner(path, backup_period = 1)
#
#   Sys.sleep(1.2)
#   runner$poll()
#
#   db_backup <- orderly_path_db_backup(path, "orderly.sqlite")
#   expect_true(file.exists(db_backup))
#
#   dat_backup <- with_sqlite(db_backup, function(con)
#     DBI::dbReadTable(con, "report_version"))
#
#   expect_equal(dat_orig, dat_backup)
# })

test_that("runner can set instance", {
  testthat::skip_on_cran()
  skip_on_windows()

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
  task_id <- runner$submit_task_report("minimal", instance = "alternative")

  result <- runner$queue$queue$task_wait(task_id)
  report_id <- result$report_id
  expect_match(result$report_id, "^\\d{8}-\\d{6}-\\w{8}")
  status <- runner$status(task_id)
  expect_equal(status$task_id, task_id)
  expect_equal(status$status, "success")
  expect_equal(status$queue, 0)
  expect_equal(status$version, report_id)
  ## Data in alternative db extracts only 10 rows
  expect_match(status$output, "\\[ data +\\]  source => dat: 10 x 2",
               all = FALSE)


  task_id_default <- runner$submit_task_report("minimal")

  result_default <- runner$queue$queue$task_wait(task_id_default)
  report_id_default <- result_default$report_id
  expect_match(result_default$report_id, "^\\d{8}-\\d{6}-\\w{8}")
  status_default <- runner$status(task_id_default)
  expect_equal(status_default$task_id, task_id_default)
  expect_equal(status_default$status, "success")
  expect_equal(status_default$queue, 0)
  expect_equal(status_default$version, report_id_default)
  ## Data in default db extracts 20 rows
  expect_match(status_default$output, "\\[ data +\\]  source => dat: 20 x 2",
               all = FALSE)
})
