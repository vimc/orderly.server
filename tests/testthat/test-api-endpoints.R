context("api - endpoints")

test_that("index", {
  endpoint <- endpoint_index(NULL)

  res <- endpoint$target()
  expect_equal(res$name, scalar("orderly.server"))
  expect_equal(res$version, version_info())
  expect_true("/v1/reports/:key/status/" %in% res$endpoints)

  runner <- mock_runner()
  api <- build_api(runner)
  res <- api$request("GET", "/")
  expect_equal(res$status, 200L)
  expect_equal(res$headers[["Content-Type"]], "application/json")
  expect_equal(res$body, as.character(endpoint$run()$body))
})


test_that("rebuild", {
  runner <- mock_runner()

  res_target <- target_rebuild(runner)
  expect_null(res_target)

  expect_simple_endpoint_runs(endpoint_rebuild(runner), res_target)
})


test_that("git_status", {
  git_status <- list(success = TRUE, code = 0, output = character(0),
                     clean = TRUE, branch = "master",
                     hash = "bc1afbf30ed83297b4da4dbb8a1930bcab746ac1")
  runner <- mock_runner(git_status = git_status)

  ## First test the basic output:
  res_target <- target_git_status(runner)
  expect_equal(mockery::mock_args(runner$git_status)[[1]], list())
  expect_equal(res_target,
               list(branch = scalar("master"),
                    hash = scalar(git_status$hash),
                    clean = scalar(TRUE),
                    output = character(0)))

  expect_simple_endpoint_runs(endpoint_git_status(runner), res_target)

  expect_equal(mockery::mock_args(runner$git_status)[[1]], list())
  expect_equal(mockery::mock_args(runner$git_status)[[2]], list())
  expect_equal(mockery::mock_args(runner$git_status)[[3]], list())
})


test_that("git_pull", {
  git_pull <- list(
    success = TRUE, code = 0,
    output = c("From upstream",
               "   0fc0d08..0ec7621  master     -> origin/master",
               "Updating 0fc0d08..0ec7621",
               "Fast-forward", " new | 1 +",
               " 1 file changed, 1 insertion(+)",
               " create mode 100644 new"))
  runner <- mock_runner(git_pull = git_pull)

  res <- target_git_pull(runner)
  expect_equal(res, git_pull$output)

  expect_simple_endpoint_runs(endpoint_git_pull(runner), res)

  expect_equal(mockery::mock_args(runner$git_pull)[[1]], list())
  expect_equal(mockery::mock_args(runner$git_pull)[[2]], list())
  expect_equal(mockery::mock_args(runner$git_pull)[[3]], list())
})


test_that("git_fetch", {
  git_fetch <- list(
    success = TRUE, code = 0,
    output = c("From upstream",
               "   0fc0d08..0ec7621  master     -> origin/master"))
  runner <- mock_runner(git_fetch = git_fetch)

  ## First test the basic output:
  res <- target_git_fetch(runner)
  expect_equal(res, git_fetch$output)

  expect_simple_endpoint_runs(endpoint_git_fetch(runner), res)

  expect_equal(mockery::mock_args(runner$git_fetch)[[1]], list())
  expect_equal(mockery::mock_args(runner$git_fetch)[[2]], list())
  expect_equal(mockery::mock_args(runner$git_fetch)[[3]], list())
})


test_that("run", {
  key <- "key-1"
  runner <- mock_runner(keys = key)

  res <- target_run(runner, "example")
  expect_equal(
    res,
    list(name = scalar("example"),
         key = scalar(key),
         path = scalar(sprintf("/v1/reports/%s/status/", key))))
  expect_equal(
    mockery::mock_args(runner$queue)[[1]],
    list("example", NULL, NULL, NULL, TRUE, timeout = 600))

  ## endpoint
  endpoint <- endpoint_run(runner)
  ## RESIDE-166: running endpoint$run() is 500 not 40x error (pkgapi bug)
  res_endpoint <- endpoint$run("example", timeout = 600)
  expect_equal(res_endpoint$status_code, 200)
  expect_equal(res_endpoint$data, res)

  ## api
  api <- build_api(runner)
  res_api <- api$request("POST", "/v1/reports/example/run/",
                         list(timeout = 600))
  expect_equal(res_api$status, 200L)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, as.character(res_endpoint$body))
})


test_that("status - queued behind nothing", {
  ## See mock.R
  key <- "key-1"
  status <- list(key = key, status = "queued", id = NA_character_,
                 output = list(stdout = character(), stderr = NULL))

  runner <- mock_runner(key, status)

  res <- target_status(runner, key)
  expect_equal(
    res,
    list(key = scalar(key),
         status = scalar("queued"),
         version = scalar(NA_character_),
         output = list(stdout = character(0), stderr = character(0))))

  expect_equal(mockery::mock_args(runner$status)[[1]], list(key, FALSE))

  ## endpoint
  endpoint <- endpoint_status(runner)
  res_endpoint <- endpoint$run(key)
  expect_equal(res_endpoint$status_code, 200)
  expect_equal(res_endpoint$content_type, "application/json")
  expect_equal(res_endpoint$data, res)
  expect_equal(mockery::mock_args(runner$status)[[2]], list(key, FALSE))

  ## api
  api <- build_api(runner)
  res_api <- api$request("GET", sprintf("/v1/reports/%s/status/", key))
  expect_equal(res_api$status, 200L)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, as.character(res_endpoint$body))
  expect_equal(mockery::mock_args(runner$status)[[3]], list(key, FALSE))
})


test_that("status - queued", {
  ## See mock.R
  key <- "key-3"
  status <- list(
    key = key, status = "queued", id = NA_character_,
    output = list(stdout = sprintf("queued:key-%d:example", 1:2),
                  stderr = NULL))

  runner <- mock_runner(key, status)

  res <- target_status(runner, key)
  expect_equal(
    res,
    list(key = scalar(key),
         status = scalar("queued"),
         version = scalar(NA_character_),
         output = list(stdout = status$output$stdout, stderr = character(0))))
  expect_equal(mockery::mock_args(runner$status)[[1]], list(key, FALSE))

  ## endpoint
  endpoint <- endpoint_status(runner)
  res_endpoint <- endpoint$run(key)
  expect_equal(res_endpoint$status_code, 200)
  expect_equal(res_endpoint$content_type, "application/json")
  expect_equal(res_endpoint$data, res)
  expect_equal(mockery::mock_args(runner$status)[[2]], list(key, FALSE))

  ## api
  api <- build_api(runner)
  res_api <- api$request("GET", sprintf("/v1/reports/%s/status/", key))
  expect_equal(res_api$status, 200L)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, as.character(res_endpoint$body))
  expect_equal(mockery::mock_args(runner$status)[[3]], list(key, FALSE))
})


test_that("status - completed, no log", {
  key <- "key-1"
  id <- "20200414-123013-a1df28f7"
  status <- list(key = key, status = "success", id = id, output = NULL)

  runner <- mock_runner(key, status)

  res <- target_status(runner, key)
  expect_equal(
    res,
    list(key = scalar(key),
         status = scalar("success"),
         version = scalar(id),
         output = NULL))
  expect_equal(mockery::mock_args(runner$status)[[1]], list(key, FALSE))

  ## endpoint
  endpoint <- endpoint_status(runner)
  res_endpoint <- endpoint$run(key)
  expect_equal(res_endpoint$status_code, 200)
  expect_equal(res_endpoint$content_type, "application/json")
  expect_equal(res_endpoint$data, res)
  expect_equal(mockery::mock_args(runner$status)[[2]], list(key, FALSE))

  ## api
  api <- build_api(runner)
  res_api <- api$request("GET", sprintf("/v1/reports/%s/status/", key))
  expect_equal(res_api$status, 200L)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, as.character(res_endpoint$body))
  expect_equal(mockery::mock_args(runner$status)[[3]], list(key, FALSE))
})


test_that("status - completed, with log", {
  key <- "key-1"
  id <- "20200414-123013-a1df28f7"
  status <- list(key = key, status = "success", id = id,
                 output = list(stdout = character(0),
                               stderr = readLines("example/success.txt")))
  runner <- mock_runner(key, status)

  res <- target_status(runner, key, TRUE)
  expect_equal(
    res,
    list(key = scalar(key),
         status = scalar("success"),
         version = scalar(id),
         output = status$output))
  expect_equal(mockery::mock_args(runner$status)[[1]], list(key, TRUE))

  ## endpoint
  endpoint <- endpoint_status(runner)
  res_endpoint <- endpoint$run(key, TRUE)
  expect_equal(res_endpoint$status_code, 200)
  expect_equal(res_endpoint$content_type, "application/json")
  expect_equal(res_endpoint$data, res)
  expect_equal(mockery::mock_args(runner$status)[[2]], list(key, TRUE))

  ## api
  api <- build_api(runner)
  res_api <- api$request("GET", sprintf("/v1/reports/%s/status/", key),
                         query = list(output = TRUE))
  expect_equal(res_api$status, 200L)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, as.character(res_endpoint$body))
  expect_equal(mockery::mock_args(runner$status)[[3]], list(key, TRUE))
})


test_that("kill - successful", {
  key <- "key-1"
  runner <- mock_runner()

  res <- target_kill(runner, key)
  expect_true(res)
  expect_equal(mockery::mock_args(runner$kill)[[1]], list(key))

  ## endpoint
  endpoint <- endpoint_kill(runner)
  res_endpoint <- endpoint$run(key)
  expect_equal(res_endpoint$status_code, 200)
  expect_equal(res_endpoint$content_type, "application/json")
  expect_equal(res_endpoint$data, res)
  expect_equal(mockery::mock_args(runner$kill)[[2]], list(key))

  ## api
  api <- build_api(runner)
  res_api <- api$request("DELETE", sprintf("/v1/reports/%s/kill/", key))
  expect_equal(res_api$status, 200L)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, as.character(res_endpoint$body))
  expect_equal(mockery::mock_args(runner$kill)[[3]], list(key))
})


test_that("kill - failure", {
  key <- "key-1"
  runner <- mock_runner()

  msg <- "Can't kill 'key-1' - not currently running a report"
  runner$kill <- mockery::mock(stop(msg), cycle = TRUE)

  res <- expect_error(target_kill(runner, key), class = "pkgapi_error")
  res$trace <- NULL
  expect_equal(mockery::mock_args(runner$kill)[[1]], list(key))
  expect_equal(res$data[[1]]$error, jsonlite::unbox("ERROR"))
  expect_equal(res$data[[1]]$detail, jsonlite::unbox(msg))

  ## endpoint
  endpoint <- endpoint_kill(runner)
  res_endpoint <- endpoint$run(key)
  expect_equal(res_endpoint$status_code, 400)
  expect_equal(res_endpoint$content_type, "application/json")
  expect_null(res_endpoint$data)
  expect_equal(res_endpoint$error, res)
  expect_equal(mockery::mock_args(runner$kill)[[2]], list(key))

  ## api
  api <- build_api(runner)
  res_api <- api$request("DELETE", sprintf("/v1/reports/%s/kill/", key))
  expect_equal(res_api$status, 400L)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, as.character(res_endpoint$body))
  expect_equal(mockery::mock_args(runner$kill)[[3]], list(key))
})


test_that("run can specify instance", {
  ## We're interested in testing that orderly.server passes instance arg
  ## to the runner$queue arg
  key <- "key-1"
  runner <- mock_runner(keys = key)

  res <- target_run(runner, "example", timeout = 100, instance = "myinstance")
  expect_equal(
    res,
    list(name = scalar("example"),
         key = scalar(key),
         path = scalar(sprintf("/v1/reports/%s/status/", key))))
  expect_equal(
    mockery::mock_args(runner$queue)[[1]],
    list("example", NULL, NULL, "myinstance", TRUE, timeout = 100))

  ## and via the api
  endpoint <- endpoint_run(runner)
  api <- build_api(runner)

  res_api <- api$request("POST", "/v1/reports/example/run/",
                         list(timeout = 100, instance = "myinstance"))
  expect_equal(
    mockery::mock_args(runner$queue)[[2]],
    list("example", NULL, NULL, "myinstance", TRUE, timeout = 100))
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

  ## test again
  api <- build_api(runner)
  res <- api$request("GET", "/run-metadata")
  expect_equal(res$status, 200L)
  expect_equal(res$headers[["Content-Type"]], "application/json")
  expect_equal(res$body, as.character(endpoint$run()$body))
})

test_that("run-metadata pulls information from runner", {
  path <- orderly::orderly_example("minimal")
  runner <- orderly::orderly_runner(path)

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
  runner <- orderly::orderly_runner(path)
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
  runner <- orderly::orderly_runner(path)
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
  runner <- orderly::orderly_runner(path)
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
    runner <- orderly::orderly_runner(path)
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

test_that("git branches endpoint", {
  path <- orderly_prepare_orderly_git_example()
  branch_data <- data.frame(
    name = c("master", "dev-branch"),
    last_commit = c("Mon Jun 1 16:00:41 2020 +0100",
                    "Mon Jun 4 12:32:41 2020 +0100"),
    last_commit_age = c(601, 1643),
    stringsAsFactors = FALSE)
  runner <- mock_runner(git_branches_no_merged = branch_data)
  endpoint <- endpoint_git_branches(runner)

  branches <- endpoint$run()
  expect_equal(branches$status_code, 200)
  expect_equal(branches$data, branch_data)
})

test_that("git commits endpoint", {
  path <- orderly_prepare_orderly_git_example()
  commit_data <- data.frame(
    id = c("2h38dns", "a2d862nd"),
    date_time = c("20-04-31 23:12:32", "20-05-23 08:54:49"),
    age = c(324, 124),
    stringsAsFactors = FALSE
  )
  runner <- mock_runner(git_commits = commit_data)
  endpoint <- endpoint_git_commits(runner)

  commits <- endpoint$run("master")
  expect_equal(commits$status_code, 200)
  expect_equal(commits$data, commit_data)
  args <- mockery::mock_args(runner$git_commits)
  expect_length(args, 1)
  expect_equal(args[[1]][[1]], "master")
})

test_that("can get available reports for a branch & commit", {
  path <- orderly_prepare_orderly_git_example()
  runner <- mock_runner(get_reports = c("report-1", "report-2"))
  endpoint <- endpoint_available_reports(runner)

  commits <- endpoint$run("master", "84hd82n")
  expect_equal(commits$status_code, 200)
  expect_equal(commits$data, c("report-1", "report-2"))
  args <- mockery::mock_args(runner$get_reports)
  expect_length(args, 1)
  expect_equal(args[[1]][[1]], "master")
  expect_equal(args[[1]][[2]], "84hd82n")
})

test_that("can get parameters for a report & commit", {
  path <- orderly_prepare_orderly_git_example()
  runner <- mock_runner(get_report_parameters = list(
    a = NULL,
    b = list(
      default = "test"
    ),
    c = list(
      default = 2
    )
  ))
  endpoint <- endpoint_report_parameters(runner)

  params <- endpoint$run("minimal", "84hd82n")
  expect_equal(params$status_code, 200)
  expect_equal(params$data, list(
    list(name = scalar("a"), default = NULL),
    list(name = scalar("b"), default = scalar("test")),
    list(name = scalar("c"), default = scalar(2))
  ))
  args <- mockery::mock_args(runner$get_report_parameters)
  expect_length(args, 1)
  expect_equal(args[[1]][[1]], "minimal")
  expect_equal(args[[1]][[2]], "84hd82n")
})

test_that("report parameters endpoint supports no parameters", {
  path <- orderly_prepare_orderly_git_example()
  runner <- mock_runner(get_report_parameters = NULL)
  endpoint <- endpoint_report_parameters(runner)

  params <- endpoint$run("minimal", "84hd82n")
  expect_equal(params$status_code, 200)
  expect_equal(params$data, list())
})

test_that("report parameter endponits handles errors", {
  path <- orderly_prepare_orderly_git_example()
  runner <- mock_runner(get_report_parameters =
                          stop("Failed to get report parameters"))
  endpoint <- endpoint_report_parameters(runner)

  params <- endpoint$run("minimal", "84hd82n")
  expect_equal(params$status_code, 400)
  expect_equal(params$error$data[[1]]$error, scalar("FAILED_RETRIEVE_PARAMS"))
  expect_equal(params$error$data[[1]]$detail,
               scalar("Failed to get report parameters"))

  ## Invalid format of parameters throws an error
  runner <- mock_runner(get_report_parameters = c("param1", "param2"))
  endpoint <- endpoint_report_parameters(runner)

  params <- endpoint$run("minimal", "84hd82n")
  expect_equal(params$status_code, 400)
  expect_equal(params$error$data[[1]]$error, scalar("INVALID_FORMAT"))
  expect_equal(params$error$data[[1]]$detail, scalar(
    "Failed to parse parameters for report 'minimal' and commit '84hd82n'"))
})
