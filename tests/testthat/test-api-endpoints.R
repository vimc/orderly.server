context("api - endpoints")

test_that("index", {
  endpoint <- endpoint_index()

  res <- endpoint$target()
  expect_equal(res$name, scalar("orderly.server"))
  expect_equal(res$version, version_info())
  expect_true("/v1/reports/:key/status/" %in% res$endpoints)

  runner <- mock_runner()
  api <- build_api(runner, "path")
  res <- api$request("GET", "/")
  expect_equal(res$status, 200L)
  expect_equal(res$headers[["Content-Type"]], "application/json")
  expect_equal(res$body, as.character(endpoint$run()$body))
})


# test_that("rebuild", {
#   runner <- mock_runner()
#
#   res_target <- target_rebuild(runner)
#   expect_null(res_target)
#
#   expect_simple_endpoint_runs(endpoint_rebuild(runner), res_target)
# })


test_that("git_status", {
  path <- orderly_unzip_git_demo()
  endpoint <- endpoint_git_status(path)

  status <- endpoint$run()
  expect_equal(status$status_code, 200)
  expect_equal(status$content_type, "application/json")
  expect_equal(names(status$data), c("branch", "hash", "clean", "output"))
  expect_equal(status$data$branch, scalar("master"))
  expect_match(status$data$hash, "^[[:xdigit:]]{40}$")
  expect_s3_class(status$data$hash, "scalar")
  expect_equal(status$data$clean, scalar(TRUE))
  expect_equal(status$data$output, character(0))

  writeLines("hello", file.path(path, "hello"))
  status <- endpoint$run()
  expect_equal(status$status_code, 200)
  expect_equal(status$content_type, "application/json")
  expect_equal(names(status$data), c("branch", "hash", "clean", "output"))
  expect_equal(status$data$branch, scalar("master"))
  expect_match(status$data$hash, "^[[:xdigit:]]{40}$")
  expect_s3_class(status$data$hash, "scalar")
  expect_equal(status$data$clean, scalar(FALSE))
  expect_equal(status$data$output, "?? hello")
})


test_that("git_pull", {
  mock_pull <- mockery::mock(list(
    success = TRUE, code = 0,
    output = c("From upstream",
               "   0fc0d08..0ec7621  master     -> origin/master",
               "Updating 0fc0d08..0ec7621",
               "Fast-forward", " new | 1 +",
               " 1 file changed, 1 insertion(+)",
               " create mode 100644 new")))

  with_mock("orderly.server:::git_pull" = mock_pull, {
    endpoint <- endpoint_git_pull("test_path")
    res <- endpoint$run()
  })

  expect_equal(res$status_code, 200)
  expect_equal(res$data, c("From upstream",
                           "   0fc0d08..0ec7621  master     -> origin/master",
                           "Updating 0fc0d08..0ec7621",
                           "Fast-forward", " new | 1 +",
                           " 1 file changed, 1 insertion(+)",
                           " create mode 100644 new"))
  mockery::expect_args(mock_pull, 1, "test_path")
})


test_that("git_fetch", {
  mock_fetch <- mockery::mock(list(
    success = TRUE, code = 0,
    output = c("From upstream",
               "   0fc0d08..0ec7621  master     -> origin/master")))

  with_mock("orderly.server:::git_fetch" = mock_fetch, {
    endpoint <- endpoint_git_fetch("test_path")
    res <- endpoint$run()
  })

  expect_equal(res$status_code, 200)
  expect_equal(res$data, c("From upstream",
                           "   0fc0d08..0ec7621  master     -> origin/master"))
  mockery::expect_args(mock_fetch, 1, "test_path")
})


test_that("git branches endpoint", {
  branch_data <- data.frame(
    name = c("master", "dev-branch"),
    last_commit = c("Mon Jun 1 16:00:41 2020 +0100",
                    "Mon Jun 4 12:32:41 2020 +0100"),
    last_commit_age = c(601, 1643),
    stringsAsFactors = FALSE)

  mock_branches <- mockery::mock(branch_data)

  with_mock("orderly.server:::git_branches_no_merged" = mock_branches, {
    endpoint <- endpoint_git_branches("test_path")
    res <- endpoint$run()
  })

  expect_equal(res$status_code, 200)
  expect_equal(res$data, branch_data)
  mockery::expect_args(mock_branches, 1, "test_path", include_master = TRUE)
})


test_that("git commits endpoint", {
  commit_data <- data.frame(
    id = c("2h38dns", "a2d862nd"),
    date_time = c("20-04-31 23:12:32", "20-05-23 08:54:49"),
    age = c(324, 124),
    stringsAsFactors = FALSE
  )
  mock_commit <- mockery::mock(commit_data)

  with_mock("orderly.server:::git_commits" = mock_commit, {
    endpoint <- endpoint_git_commits("test_path")
    res <- endpoint$run(branch = "master")
  })

  expect_equal(res$status_code, 200)
  expect_equal(res$data, commit_data)
  mockery::expect_args(mock_commit, 1, "master", "test_path")
})

test_that("can get available reports for a branch & commit", {
  path <- orderly_prepare_orderly_git_example()
  endpoint <- endpoint_available_reports(path["origin"])
  ref <- substr(git_ref_to_sha("master", path["origin"]), 1, 7)

  res <- endpoint$run("master", ref)
  expect_equal(res$status_code, 200)
  expect_equal(res$data, c("global", "minimal"))
})

test_that("can get parameters for a report & commit", {
  mock_report_parameters <- mockery::mock(list(
    a = NULL,
    b = list(
      default = "test"
    ),
    c = list(
      default = 2
    )
  ))

  with_mock("orderly.server:::get_report_parameters" = mock_report_parameters, {
    endpoint <- endpoint_report_parameters("test_path")
    res <- endpoint$run(report_id = "id", commit = "1234567")
  })

  expect_equal(res$status_code, 200)
  expect_equal(res$data, list(
    list(
      name = scalar("a"),
      default = NULL
    ),
    list(
      name = scalar("b"),
      default = scalar("test")
    ),
    list(
      name = scalar("c"),
      default = scalar("2")
    )
  ))
  mockery::expect_args(mock_report_parameters, 1, "id", "1234567", "test_path")
})

test_that("report parameters endpoint supports no parameters", {
  path <- orderly_prepare_orderly_git_example()
  endpoint <- endpoint_report_parameters(path["origin"])
  ref <- substr(git_ref_to_sha("master", path["origin"]), 1, 7)

  res <- endpoint$run(report_id = "minimal", commit = ref)
  expect_equal(res$status_code, 200)
  expect_equal(res$data, list())
})

test_that("report parameter endponits handles errors", {
  mock_report_parameters <- mockery::mock(
    stop("Failed to get report parameters"))

  with_mock("orderly.server:::get_report_parameters" = mock_report_parameters, {
    endpoint <- endpoint_report_parameters("test_path")
    res <- endpoint$run(report_id = "id", commit = "1234567")
  })
  expect_equal(res$status_code, 400)
  expect_equal(res$error$data[[1]]$error, scalar("FAILED_RETRIEVE_PARAMS"))
  expect_equal(res$error$data[[1]]$detail,
               scalar("Failed to get report parameters"))

  ## Invalid format of parameters throws an error
  mock_report_parameters <- mockery::mock(c("param1", "param2"))

  with_mock("orderly.server:::get_report_parameters" = mock_report_parameters, {
    endpoint <- endpoint_report_parameters("test_path")
    res <- endpoint$run(report_id = "id", commit = "1234567")
  })
  expect_equal(res$status_code, 400)
  expect_equal(res$error$data[[1]]$error, scalar("INVALID_FORMAT"))
  expect_equal(res$error$data[[1]]$detail, scalar(
    "Failed to parse parameters for report 'id' and commit '1234567'"))
})


test_that("run", {
  task_id <- "task-id-1"
  runner <- mock_runner(task_ids = task_id)

  res <- target_run(runner, "example")
  expect_equal(
    res,
    list(name = scalar("example"),
         key = scalar(task_id),
         path = scalar(sprintf("/v1/reports/%s/status/", task_id))))
  expect_equal(
    mockery::mock_args(runner$submit_task_report)[[1]],
    list("example", NULL, NULL, NULL, timeout = 600))

  ## endpoint
  endpoint <- endpoint_run(runner)
  ## RESIDE-166: running endpoint$run() is 500 not 40x error (pkgapi bug)
  res_endpoint <- endpoint$run("example", timeout = 600)
  expect_equal(res_endpoint$status_code, 200)
  expect_equal(res_endpoint$data, res)

  ## api
  api <- build_api(runner, "path")
  res_api <- api$request("POST", "/v1/reports/example/run/",
                         list(timeout = 600))
  expect_equal(res_api$status, 200L)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, as.character(res_endpoint$body))
})


test_that("status - queued behind nothing", {
  ## See mock.R
  task_id <- "task-id-2"
  status <- list(task_id = task_id, status = "queued")

  runner <- mock_runner(task_id, status)

  res <- target_status(runner, task_id)
  expect_equal(
    res,
    list(key = scalar(task_id),
         status = scalar("queued"),
         version = NULL,
         output = list()))

  expect_equal(mockery::mock_args(runner$status)[[1]], list(task_id, FALSE))

  ## endpoint
  endpoint <- endpoint_status(runner)
  res_endpoint <- endpoint$run(task_id)
  expect_equal(res_endpoint$status_code, 200)
  expect_equal(res_endpoint$content_type, "application/json")
  expect_equal(res_endpoint$data, res)
  expect_equal(mockery::mock_args(runner$status)[[2]], list(task_id, FALSE))

  ## api
  api <- build_api(runner, "path")
  res_api <- api$request("GET", sprintf("/v1/reports/%s/status/", task_id))
  expect_equal(res_api$status, 200L)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, as.character(res_endpoint$body))
  expect_equal(mockery::mock_args(runner$status)[[3]], list(task_id, FALSE))
})


test_that("status - queued", {
  ## See mock.R
  task_id <- "task-id-3"
  status <- list(
    task_id = task_id, status = "queued", version = NA_character_,
    output = sprintf("queued:key-%d:example", 1:2))

  runner <- mock_runner(task_id, status)

  res <- target_status(runner, task_id)
  expect_equal(
    res,
    list(key = scalar(task_id),
         status = scalar("queued"),
         version = scalar(NA_character_),
         output = list(scalar("queued:key-1:example"),
                       scalar("queued:key-2:example"))))
  expect_equal(mockery::mock_args(runner$status)[[1]], list(task_id, FALSE))

  ## endpoint
  endpoint <- endpoint_status(runner)
  res_endpoint <- endpoint$run(task_id)
  expect_equal(res_endpoint$status_code, 200)
  expect_equal(res_endpoint$content_type, "application/json")
  expect_equal(res_endpoint$data, res)
  expect_equal(mockery::mock_args(runner$status)[[2]], list(task_id, FALSE))

  ## api
  api <- build_api(runner, "path")
  res_api <- api$request("GET", sprintf("/v1/reports/%s/status/", task_id))
  expect_equal(res_api$status, 200L)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, as.character(res_endpoint$body))
  expect_equal(mockery::mock_args(runner$status)[[3]], list(task_id, FALSE))
})


test_that("status - completed, no log", {
  task_id <- "task-id-1"
  version <- "20200414-123013-a1df28f7"
  status <- list(task_id = task_id, status = "success", version = version,
                 output = NULL)

  runner <- mock_runner(task_id, status)

  res <- target_status(runner, task_id)
  expect_equal(
    res,
    list(key = scalar(task_id),
         status = scalar("success"),
         version = scalar(version),
         output = NULL))
  expect_equal(mockery::mock_args(runner$status)[[1]], list(task_id, FALSE))

  ## endpoint
  endpoint <- endpoint_status(runner)
  res_endpoint <- endpoint$run(task_id)
  expect_equal(res_endpoint$status_code, 200)
  expect_equal(res_endpoint$content_type, "application/json")
  expect_equal(res_endpoint$data, res)
  expect_equal(mockery::mock_args(runner$status)[[2]], list(task_id, FALSE))

  ## api
  api <- build_api(runner, "path")
  res_api <- api$request("GET", sprintf("/v1/reports/%s/status/", task_id))
  expect_equal(res_api$status, 200L)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, as.character(res_endpoint$body))
  expect_equal(mockery::mock_args(runner$status)[[3]], list(task_id, FALSE))
})


test_that("status - completed, with log", {
  task_id <- "task-id-1"
  version <- "20200414-123013-a1df28f7"
  status <- list(task_id = task_id, status = "success", version = version,
                 output = "example/success.txt")
  runner <- mock_runner(task_id, status)

  res <- target_status(runner, task_id, TRUE)
  expect_equal(
    res,
    list(key = scalar(task_id),
         status = scalar("success"),
         version = scalar(version),
         output = list(scalar("example/success.txt"))))
  expect_equal(mockery::mock_args(runner$status)[[1]], list(task_id, TRUE))

  ## endpoint
  endpoint <- endpoint_status(runner)
  res_endpoint <- endpoint$run(task_id, TRUE)
  expect_equal(res_endpoint$status_code, 200)
  expect_equal(res_endpoint$content_type, "application/json")
  expect_equal(res_endpoint$data, res)
  expect_equal(mockery::mock_args(runner$status)[[2]], list(task_id, TRUE))

  ## api
  api <- build_api(runner, "path")
  res_api <- api$request("GET", sprintf("/v1/reports/%s/status/", task_id),
                         query = list(output = TRUE))
  expect_equal(res_api$status, 200L)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, as.character(res_endpoint$body))
  expect_equal(mockery::mock_args(runner$status)[[3]], list(task_id, TRUE))
})

#
# test_that("kill - successful", {
#   key <- "key-1"
#   runner <- mock_runner()
#
#   res <- target_kill(runner, key)
#   expect_true(res)
#   expect_equal(mockery::mock_args(runner$kill)[[1]], list(key))
#
#   ## endpoint
#   endpoint <- endpoint_kill(runner)
#   res_endpoint <- endpoint$run(key)
#   expect_equal(res_endpoint$status_code, 200)
#   expect_equal(res_endpoint$content_type, "application/json")
#   expect_equal(res_endpoint$data, res)
#   expect_equal(mockery::mock_args(runner$kill)[[2]], list(key))
#
#   ## api
#   api <- build_api(runner)
#   res_api <- api$request("DELETE", sprintf("/v1/reports/%s/kill/", key))
#   expect_equal(res_api$status, 200L)
#   expect_equal(res_api$headers[["Content-Type"]], "application/json")
#   expect_equal(res_api$body, as.character(res_endpoint$body))
#   expect_equal(mockery::mock_args(runner$kill)[[3]], list(key))
# })
#
#
# test_that("kill - failure", {
#   key <- "key-1"
#   runner <- mock_runner()
#
#   msg <- "Can't kill 'key-1' - not currently running a report"
#   runner$kill <- mockery::mock(stop(msg), cycle = TRUE)
#
#   res <- expect_error(target_kill(runner, key), class = "pkgapi_error")
#   res$trace <- NULL
#   expect_equal(mockery::mock_args(runner$kill)[[1]], list(key))
#   expect_equal(res$data[[1]]$error, jsonlite::unbox("ERROR"))
#   expect_equal(res$data[[1]]$detail, jsonlite::unbox(msg))
#
#   ## endpoint
#   endpoint <- endpoint_kill(runner)
#   res_endpoint <- endpoint$run(key)
#   expect_equal(res_endpoint$status_code, 400)
#   expect_equal(res_endpoint$content_type, "application/json")
#   expect_null(res_endpoint$data)
#   expect_equal(res_endpoint$error, res)
#   expect_equal(mockery::mock_args(runner$kill)[[2]], list(key))
#
#   ## api
#   api <- build_api(runner)
#   res_api <- api$request("DELETE", sprintf("/v1/reports/%s/kill/", key))
#   expect_equal(res_api$status, 400L)
#   expect_equal(res_api$headers[["Content-Type"]], "application/json")
#   expect_equal(res_api$body, as.character(res_endpoint$body))
#   expect_equal(mockery::mock_args(runner$kill)[[3]], list(key))
# })
#
#
test_that("run can specify instance", {
  ## We're interested in testing that orderly.server passes instance arg
  ## to the runner$queue arg
  task_id <- "task-id-1"
  runner <- mock_runner(task_ids = task_id)

  res <- target_run(runner, "example", timeout = 100, instance = "myinstance")
  expect_equal(
    res,
    list(name = scalar("example"),
         key = scalar(task_id),
         path = scalar(sprintf("/v1/reports/%s/status/", task_id))))
  expect_equal(
    mockery::mock_args(runner$submit_task_report)[[1]],
    list("example", NULL, NULL, "myinstance", timeout = 100))

  ## and via the api
  api <- build_api(runner, "path")

  res_api <- api$request("POST", "/v1/reports/example/run/",
                         list(timeout = 100, instance = "myinstance"))
  expect_equal(
    mockery::mock_args(runner$submit_task_report)[[2]],
    list("example", NULL, NULL, "myinstance", timeout = 100))
})

test_that("run-metadata", {
  config <- list(
    changelog = data.frame(
      id = c("internal", "public"),
      public = c(FALSE, TRUE),
      stringsAsFactors = FALSE
    ),
    database = list(
      source = list(
        instances = list(
          production = list(
            dbname = "production.sqlite"
          ),
          staging = list(
            dbname = "staging.sqlite"
          )
        )
      )
    ),
    server_options = function() {
      NULL
    }
  )
  runner <- mock_runner(config = config)
  endpoint <- endpoint_run_metadata(runner)

  res <- endpoint$target()
  expect_equal(res$instances_supported, scalar(TRUE))
  expect_equal(res$git_supported, scalar(TRUE))
  expect_equal(res$instances,
               list(source = c(scalar("production"), scalar("staging"))))
  expect_equal(res$changelog_types, c(scalar("public")))

  ## test through API
  api <- build_api(runner, "path")
  res <- api$request("GET", "/run-metadata")
  expect_equal(res$status, 200L)
  expect_equal(res$headers[["Content-Type"]], "application/json")
  expect_equal(res$body, as.character(endpoint$run()$body))
})

test_that("run-metadata pulls information from runner", {
  skip_if_no_redis()
  path <- orderly::orderly_example("minimal")
  runner <- orderly_runner(path, workers = 0)

  expect_equal(target_run_metadata(runner), list(
    name = NULL,
    instances_supported = scalar(FALSE),
    git_supported = scalar(FALSE),
    instances = list(
      "source" = character(0)
    ),
    changelog_types = NULL
  ))

  ## Example with all enabled
  path <- orderly::orderly_example("minimal")
  yml <- c("database:",
           "  source:",
           "    driver: RSQLite::SQLite",
           "    args:",
           "      dbname: source.sqlite",
           "      user: user",
           "    instances:",
           "      production:",
           "        host: production.montagu.dide.ic.ac.uk",
           "        port: 5432",
           "        password: pwd",
           "      staging:",
           "        host: support.montagu.dide.ic.ac.uk",
           "        port: 5432",
           "        password: pwd",
           "changelog:",
           "  internal:",
           "    public: false",
           "  external:",
           "    public: true"
           )
  writeLines(yml, file.path(path, "orderly_config.yml"))
  runner <- orderly_runner(path, workers = 0)
  expect_equal(target_run_metadata(runner), list(
    name = NULL,
    instances_supported = scalar(TRUE),
    git_supported = scalar(FALSE),
    instances = list(
      "source" = c(scalar("production"), scalar("staging"))
    ),
    changelog_types = c(scalar("external"))
  ))
})

test_that("run-metadata can get config for multiple databases", {
  skip_if_no_redis()
  path <- orderly::orderly_example("minimal")
  yml <- c("database:",
           "  source:",
           "    driver: RSQLite::SQLite",
           "    args:",
           "      dbname: source.sqlite",
           "      user: user",
           "    instances:",
           "      production:",
           "        host: production.montagu.dide.ic.ac.uk",
           "        port: 5432",
           "        password: pwd",
           "      staging:",
           "        host: support.montagu.dide.ic.ac.uk",
           "        port: 5432",
           "        password: pwd",
           "  annex:",
           "    driver: RPostgres::Postgres",
           "    args:",
           "      dbname: name",
           "      user: readonly",
           "      host: annex_host",
           "      port: 1234",
           "      password: PW"
  )
  writeLines(yml, file.path(path, "orderly_config.yml"))
  runner <- orderly_runner(path, workers = 0)
  expect_equal(target_run_metadata(runner), list(
    name = NULL,
    instances_supported = scalar(TRUE),
    git_supported = scalar(FALSE),
    instances = list(
      source = c(scalar("production"), scalar("staging")),
      annex = character(0)
    ),
    changelog_types = NULL
  ))

  path <- orderly::orderly_example("minimal")
  yml <- c("database:",
           "  source:",
           "    driver: RSQLite::SQLite",
           "    args:",
           "      dbname: source.sqlite",
           "      user: user",
           "    instances:",
           "      production:",
           "        host: production.montagu.dide.ic.ac.uk",
           "        port: 5432",
           "        password: pwd",
           "      staging:",
           "        host: support.montagu.dide.ic.ac.uk",
           "        port: 5432",
           "        password: pwd",
           "  annex:",
           "    driver: RPostgres::Postgres",
           "    args:",
           "      dbname: name",
           "      user: readonly",
           "    instances:",
           "      annex1:",
           "        host: annex1_host",
           "        port: 1234",
           "        password: PW",
           "      annex2:",
           "        host: annex2_host",
           "        port: 12345",
           "        password: PW2"
  )
  writeLines(yml, file.path(path, "orderly_config.yml"))
  runner <- orderly_runner(path, workers = 0)
  expect_equal(target_run_metadata(runner), list(
    name = NULL,
    instances_supported = scalar(TRUE),
    git_supported = scalar(FALSE),
    instances = list(
      source = c(scalar("production"), scalar("staging")),
      annex = c(scalar("annex1"), scalar("annex2"))
    ),
    changelog_types = NULL
  ))
})

test_that("run metadata can get name from config", {
  skip_if_no_redis()
  path <- orderly::orderly_example("minimal")
  yml <- c("database:",
           "  source:",
           "    driver: RSQLite::SQLite",
           "    args:",
           "      dbname: source.sqlite",
           "      user: user",
           "    instances:",
           "      production:",
           "        host: production.montagu.dide.ic.ac.uk",
           "        port: 5432",
           "        password: pwd",
           "      staging:",
           "        host: support.montagu.dide.ic.ac.uk",
           "        port: 5432",
           "        password: pwd",
           "remote:",
           "  production:",
           "    driver: RSQLite::SQLite",
           "    args:",
           "      dbname: source.sqlite",
           "      user: user",
           "      host: production.montagu.dide.ic.ac.uk",
           "      port: 5432",
           "      password: pwd",
           "    slack_url: slack_url",
           "    teams_url: teams_url",
           "  staging:",
           "    driver: RSQLite::SQLite",
           "    args:",
           "        host: support.montagu.dide.ic.ac.uk",
           "        port: 5432",
           "        password: pwd",
           "    slack_url: slack_url",
           "    teams_url: teams_url"
  )
  writeLines(yml, file.path(path, "orderly_config.yml"))
  withr::with_envvar(c("ORDERLY_API_SERVER_IDENTITY" = "production"), {
    runner <- orderly_runner(path, workers = 0)
    metadata <- target_run_metadata(runner)
  })
  expect_equal(metadata, list(
    name = scalar("production"),
    instances_supported = scalar(TRUE),
    git_supported = scalar(FALSE),
    instances = list(
      source = c(scalar("production"), scalar("staging"))
    ),
    changelog_types = NULL
  ))
})
