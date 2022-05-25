context("api - endpoints")

test_that("root", {
  endpoint <- endpoint_root()

  res <- endpoint$target()
  expect_equal(res$name, scalar("orderly.server"))
  expect_equal(res$version, version_info())

  runner <- mock_runner()
  api <- build_api(runner, "path")
  res <- api$request("GET", "/")
  expect_equal(res$status, 200L)
  expect_equal(res$headers[["Content-Type"]], "application/json")
  expect_equal(res$body, as.character(endpoint$run()$body))
})


test_that("git_status", {
  path <- orderly_unzip_git_demo()
  runner <- mock_runner(root = path)
  endpoint <- endpoint_git_status(runner)

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


test_that("git_status with non-master repo", {
  path <- orderly_unzip_git_demo(default_branch = "main")
  runner <- mock_runner(root = path, default_branch = "main")
  endpoint <- endpoint_git_status(runner)

  status <- endpoint$run()
  expect_equal(status$status_code, 200)
  expect_equal(status$content_type, "application/json")
  expect_equal(names(status$data), c("branch", "hash", "clean", "output"))
  expect_equal(status$data$branch, scalar("main"))
  expect_match(status$data$hash, "^[[:xdigit:]]{40}$")
  expect_s3_class(status$data$hash, "scalar")
  expect_equal(status$data$clean, scalar(TRUE))
  expect_equal(status$data$output, character(0))

  writeLines("hello", file.path(path, "hello"))
  status <- endpoint$run()
  expect_equal(status$status_code, 200)
  expect_equal(status$content_type, "application/json")
  expect_equal(names(status$data), c("branch", "hash", "clean", "output"))
  expect_equal(status$data$branch, scalar("main"))
  expect_match(status$data$hash, "^[[:xdigit:]]{40}$")
  expect_s3_class(status$data$hash, "scalar")
  expect_equal(status$data$clean, scalar(FALSE))
  expect_equal(status$data$output, "?? hello")
})


test_that("git_pull", {
  path <- orderly_prepare_orderly_git_example()
  runner <- mock_runner(root = path[["local"]])
  endpoint <- endpoint_git_pull(runner)
  res <- endpoint$run()
  expect_equal(res$status_code, 200)
  expect_match(res$data, "Fast-forward", all = FALSE)
  expect_equal(
    git_ref_to_sha("HEAD", path[["local"]]),
    git_ref_to_sha("HEAD", path[["origin"]]))
})


test_that("git_fetch", {
  path <- orderly_prepare_orderly_git_example()
  runner <- mock_runner(root = path[["local"]])
  endpoint <- endpoint_git_fetch(runner)
  res <- endpoint$run()
  expect_equal(res$status_code, 200)

  expect_equal(
    git_ref_to_sha("origin/master", path[["local"]]),
    git_ref_to_sha("HEAD", path[["origin"]]))
  expect_equal(
    git_ref_to_sha("HEAD", path[["local"]]),
    git_ref_to_sha("master^", path[["origin"]]))
})


test_that("git branches endpoint", {
  path <- orderly_prepare_orderly_git_example()
  runner <- mock_runner(root = path[["local"]])
  endpoint <- endpoint_git_branches(runner)
  res <- endpoint$run()

  expect_equal(res$status_code, 200)
  expect_equal(names(res$data), c("name", "last_commit", "last_commit_age"))
  expect_setequal(res$data$name, c("master", "other"))
})


test_that("git commits endpoint", {
  path <- orderly_prepare_orderly_git_example()
  runner <- mock_runner(root = path[["local"]])
  endpoint <- endpoint_git_commits(runner)
  res <- endpoint$run("master")

  data <- git_commits("master", runner$root, "master")
  expect_equal(res$status_code, 200)
  expect_equal(res$data[c("id", "date_time")],
               data[c("id", "date_time")])
  expect_lte(res$data$age, data$age)
})

test_that("can get available reports for a branch & commit", {
  path <- orderly_prepare_orderly_git_example()
  runner <- mock_runner(root = path[["origin"]])
  endpoint <- endpoint_available_reports(runner)
  ref <- substr(git_ref_to_sha("master", path["origin"]), 1, 7)

  res <- endpoint$run("master", ref)
  expect_equal(res$status_code, 200)
  expect_equal(res$data, c("global", "minimal"))
})

test_that("can get parameters for a report & commit", {
  path <- orderly_prepare_orderly_git_example()
  runner <- mock_runner(root = path[["origin"]])
  endpoint <- endpoint_report_parameters(runner)

  writeLines(c(
    "parameters:",
    "  a: ~",
    "  b:",
    "    default: test",
    "  c:",
    "    default: 2"),
    file.path(path[["origin"]], "src", "minimal", "orderly.yml"))
  gert::git_add(".", repo = path[["origin"]])
  ref <- gert::git_commit("add parameters", repo = path[["origin"]])

  res <- endpoint$run("minimal", ref)

  expect_equal(res$status_code, 200)
  expect_equal(res$data, list(
    list(
      name = scalar("a"),
      value = NULL
    ),
    list(
      name = scalar("b"),
      value = scalar("test")
    ),
    list(
      name = scalar("c"),
      value = scalar("2")
    )
  ))
})

test_that("report parameters endpoint supports no parameters", {
  path <- orderly_prepare_orderly_git_example()
  runner <- mock_runner(root = path[["origin"]])
  endpoint <- endpoint_report_parameters(runner)
  ref <- substr(git_ref_to_sha("master", path["origin"]), 1, 7)

  res <- endpoint$run(report_id = "minimal", commit = ref)
  expect_equal(res$status_code, 200)
  expect_equal(res$data, list())
})

test_that("report parameter endpoints handles errors", {
  path <- orderly_prepare_orderly_git_example()
  runner <- mock_runner(root = path[["origin"]])
  endpoint <- endpoint_report_parameters(runner)

  res <- endpoint$run(report_id = "minimal", commit = "1234567")
  expect_equal(res$status_code, 400)
  expect_equal(res$error$data[[1]]$error, scalar("FAILED_RETRIEVE_PARAMS"))
  expect_match(res$error$data[[1]]$detail,
               "Failed to get report parameters")
})


test_that("Invalid format of parameters throws an error", {
  path <- orderly_prepare_orderly_git_example()
  runner <- mock_runner(root = path[["origin"]])
  endpoint <- endpoint_report_parameters(runner)

  writeLines(c("parameters:", "  - a", "  - b"),
    file.path(path[["origin"]], "src", "minimal", "orderly.yml"))
  gert::git_add(".", repo = path[["origin"]])
  ref <- gert::git_commit("add parameters", repo = path[["origin"]])

  res <- endpoint$run("minimal", ref)

  expect_equal(res$status_code, 400)
  expect_equal(res$error$data[[1]]$error, scalar("INVALID_FORMAT"))
  expect_match(
    res$error$data[[1]]$detail,
    "Failed to parse parameters for report 'minimal'")
})


test_that("run", {
  key <- "key-1"
  runner <- mock_runner(key = key)

  res <- target_run(runner, "example")
  expect_equal(
    res,
    list(name = scalar("example"),
         key = scalar(key),
         path = scalar(sprintf("/v1/reports/%s/status/", key))))
  expect_equal(
    mockery::mock_args(runner$submit_task_report)[[1]],
    list("example", NULL, NULL, NULL, changelog = NULL, timeout = 60 * 60 * 3))

  ## endpoint
  endpoint <- endpoint_run(runner)
  ## RESIDE-166: running endpoint$run() is 500 not 40x error (porcelain bug)
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

test_that("run with parameters", {
  key <- "key-1"
  runner <- mock_runner(key = key)

  res <- target_run(runner, "example", body = '{"params": {"a": 1}}')
  expect_equal(
    res,
    list(name = scalar("example"),
         key = scalar(key),
         path = scalar(sprintf("/v1/reports/%s/status/", key))))
  expect_equal(
    mockery::mock_args(runner$submit_task_report)[[1]],
    list("example", list(a = 1), NULL, NULL, changelog = NULL,
         timeout = 60 * 60 * 3))
})


test_that("run with changelog", {
  key <- "key-1"
  runner <- mock_runner(key = key)

  res <- target_run(runner, "example",
                    body = '{"changelog": {"type": "test", "message": "msg"}}')
  expect_equal(
    res,
    list(name = scalar("example"),
         key = scalar(key),
         path = scalar(sprintf("/v1/reports/%s/status/", key))))
  expect_equal(
    mockery::mock_args(runner$submit_task_report)[[1]],
    list("example", NULL, NULL, NULL, changelog = "[test] msg",
         timeout = 60 * 60 * 3))
})


test_that("status - queued behind nothing", {
  ## See mock.R
  key <- "key-2"
  status <- list(key = key, status = "queued", version = NULL,
                 start_time = NULL, queue = NULL)

  runner <- mock_runner(key, status)

  res <- target_status(runner, key)
  expect_equal(
    res,
    list(key = scalar(key),
         status = scalar("queued"),
         version = NULL,
         start_time = NULL,
         output = NULL,
         queue = list()))

  expect_equal(mockery::mock_args(runner$status)[[1]], list(key, FALSE))

  ## endpoint
  endpoint <- endpoint_status(runner)
  res_endpoint <- endpoint$run(key)
  expect_equal(res_endpoint$status_code, 200)
  expect_equal(res_endpoint$content_type, "application/json")
  expect_equal(res_endpoint$data, res)
  expect_equal(mockery::mock_args(runner$status)[[2]], list(key, FALSE))

  ## api
  api <- build_api(runner, "path")
  res_api <- api$request("GET", sprintf("/v1/reports/%s/status/", key))
  expect_equal(res_api$status, 200L)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, as.character(res_endpoint$body))
  expect_equal(mockery::mock_args(runner$status)[[3]], list(key, FALSE))
})


test_that("status - queued", {
  key <- "key-3"
  status <- list(
    key = key, status = "queued", version = NA_character_, start_time = NULL,
    queue = list(
      list(
        key = "key-1",
        status = "running",
        name = "minimal",
        version = "20210310-123928-fef89bc7"
      ),
      list(
        key = "key-2",
        status = "queued",
        name = "minimal",
        version = NULL)))

  runner <- mock_runner(key, status)

  res <- target_status(runner, key)
  expect_equal(
    res,
    list(key = scalar(key),
         status = scalar("queued"),
         version = scalar(NA_character_),
         start_time = NULL,
         output = NULL,
         queue = list(
           list(
             key = scalar("key-1"),
             status = scalar("running"),
             name = scalar("minimal"),
             version = scalar("20210310-123928-fef89bc7")
           ),
           list(
             key = scalar("key-2"),
             status = scalar("queued"),
             name = scalar("minimal"),
             version = NULL))))
  expect_equal(mockery::mock_args(runner$status)[[1]], list(key, FALSE))

  ## endpoint
  endpoint <- endpoint_status(runner)
  res_endpoint <- endpoint$run(key)
  expect_equal(res_endpoint$status_code, 200)
  expect_equal(res_endpoint$content_type, "application/json")
  expect_equal(res_endpoint$data, res)
  expect_equal(mockery::mock_args(runner$status)[[2]], list(key, FALSE))

  ## api
  api <- build_api(runner, "path")
  res_api <- api$request("GET", sprintf("/v1/reports/%s/status/", key))
  expect_equal(res_api$status, 200L)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, as.character(res_endpoint$body))
  expect_equal(mockery::mock_args(runner$status)[[3]], list(key, FALSE))
})


test_that("status - completed, no log", {
  key <- "key-1"
  version <- "20200414-123013-a1df28f7"
  start_time <- as.numeric(Sys.time())
  status <- list(key = key, status = "success", version = version,
                 start_time = start_time, output = NULL, queue = character(0))

  runner <- mock_runner(key, status)

  res <- target_status(runner, key)
  expect_equal(
    res,
    list(key = scalar(key),
         status = scalar("success"),
         version = scalar(version),
         start_time = scalar(start_time),
         output = NULL,
         queue = list()))
  expect_equal(mockery::mock_args(runner$status)[[1]], list(key, FALSE))

  ## endpoint
  endpoint <- endpoint_status(runner)
  res_endpoint <- endpoint$run(key)
  expect_equal(res_endpoint$status_code, 200)
  expect_equal(res_endpoint$content_type, "application/json")
  expect_equal(res_endpoint$data, res)
  expect_equal(mockery::mock_args(runner$status)[[2]], list(key, FALSE))

  ## api
  api <- build_api(runner, "path")
  res_api <- api$request("GET", sprintf("/v1/reports/%s/status/", key))
  expect_equal(res_api$status, 200L)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, as.character(res_endpoint$body))
  expect_equal(mockery::mock_args(runner$status)[[3]], list(key, FALSE))
})


test_that("status - completed, with log", {
  key <- "key-1"
  version <- "20200414-123013-a1df28f7"
  start_time <- as.numeric(Sys.time())
  status <- list(key = key, status = "success", version = version,
                 start_time = start_time,
                 output = c("a message", "in the logs"),
                 queue = character(0))
  runner <- mock_runner(key, status)

  res <- target_status(runner, key, TRUE)
  expect_equal(
    res,
    list(key = scalar(key),
         status = scalar("success"),
         version = scalar(version),
         start_time = scalar(start_time),
         output = c("a message", "in the logs"),
         queue = list()))
  expect_equal(mockery::mock_args(runner$status)[[1]], list(key, TRUE))

  ## endpoint
  endpoint <- endpoint_status(runner)
  res_endpoint <- endpoint$run(key, TRUE)
  expect_equal(res_endpoint$status_code, 200)
  expect_equal(res_endpoint$content_type, "application/json")
  expect_equal(res_endpoint$data, res)
  expect_equal(mockery::mock_args(runner$status)[[2]], list(key, TRUE))

  ## api
  api <- build_api(runner, "path")
  res_api <- api$request("GET", sprintf("/v1/reports/%s/status/", key),
                         query = list(output = TRUE))
  expect_equal(res_api$status, 200L)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, as.character(res_endpoint$body))
  expect_equal(mockery::mock_args(runner$status)[[3]], list(key, TRUE))
})


test_that("queue status", {
  queue_status <- list(
    tasks = list(
      list(
        key = "key-1",
        status = "running",
        version = "20210310-123928-fef89bc7",
        inputs = list(
          name = "minimal",
          params = list(timeout = 10, poll = 1),
          ref = NULL,
          instance = NULL,
          changelog = "[internal] changelog")
      ),
      list(
        key = "key-2",
        status = "queued",
        version = NULL,
        inputs = list(
          name = "minimal",
          params = NULL,
          ref = "123",
          instance = "production",
          changelog = NULL))
    )
  )

  runner <- mock_runner(queue_status = queue_status)

  res <- target_queue_status(runner)
  expect_equal(
    res,
    list(tasks = list(
      list(
        key = scalar("key-1"),
        status = scalar("running"),
        version = scalar("20210310-123928-fef89bc7"),
        inputs = list(
          name = scalar("minimal"),
          params = list(timeout = scalar(10), poll = scalar(1)),
          ref = NULL,
          instance = NULL,
          changelog = scalar("[internal] changelog"))
      ),
      list(
        key = scalar("key-2"),
        status = scalar("queued"),
        version = NULL,
        inputs = list(
          name = scalar("minimal"),
          params = NULL,
          ref = scalar("123"),
          instance = scalar("production"),
          changelog = NULL)))))
  mockery::expect_called(runner$queue_status, 1)

  ## endpoint
  endpoint <- endpoint_queue_status(runner)
  res_endpoint <- endpoint$run()
  expect_equal(res_endpoint$status_code, 200)
  expect_equal(res_endpoint$content_type, "application/json")
  expect_equal(res_endpoint$data, res)
  mockery::expect_called(runner$queue_status, 2)

  ## api
  api <- build_api(runner, "path")
  res_api <- api$request("GET", "/v1/queue/status/")
  expect_equal(res_api$status, 200L)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, as.character(res_endpoint$body))
  mockery::expect_called(runner$queue_status, 3)
})


test_that("kill - successful", {
  key <- "key-1"
  runner <- mock_runner()

  res <- target_kill(runner, key)
  expect_true(res$killed)
  expect_equal(res$message, scalar(NA_character_))
  expect_equal(mockery::mock_args(runner$kill)[[1]], list(key))

  ## endpoint
  endpoint <- endpoint_kill(runner)
  res_endpoint <- endpoint$run(key)
  expect_equal(res_endpoint$status_code, 200)
  expect_equal(res_endpoint$content_type, "application/json")
  expect_equal(res_endpoint$data, res)
  expect_equal(mockery::mock_args(runner$kill)[[2]], list(key))

  ## api
  api <- build_api(runner, runner$root)
  res_api <- api$request("DELETE", sprintf("/v1/reports/%s/kill/", key))
  expect_equal(res_api$status, 200L)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, as.character(res_endpoint$body))
  expect_equal(mockery::mock_args(runner$kill)[[3]], list(key))
})


test_that("kill - failure", {
  testthat::skip_on_cran()
  skip_on_windows()
  skip_if_no_redis()
  path <- orderly_git_example("minimal")
  runner <- orderly_runner(path)

  key <- "key-1"
  res <- target_kill(runner, key)
  expect_false(res$killed)
  expect_equal(res$message,
               scalar("Failed to kill 'key-1'\n   task doesn't exist"))

  ## endpoint
  endpoint <- endpoint_kill(runner)
  res_endpoint <- endpoint$run(key)
  expect_equal(res_endpoint$status_code, 200)
  expect_equal(res_endpoint$content_type, "application/json")
  expect_equal(res_endpoint$data, res)

  ## api
  api <- build_api(runner, runner$root)
  res_api <- api$request("DELETE", sprintf("/v1/reports/%s/kill/", key))
  expect_equal(res_api$status, 200L)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, as.character(res_endpoint$body))
})


test_that("run can specify instance", {
  ## We're interested in testing that orderly.server passes instance arg
  ## to the runner$queue arg
  key <- "key-1"
  runner <- mock_runner(key = key)

  res <- target_run(runner, "example", timeout = 100, instance = "myinstance")
  expect_equal(
    res,
    list(name = scalar("example"),
         key = scalar(key),
         path = scalar(sprintf("/v1/reports/%s/status/", key))))
  expect_equal(
    mockery::mock_args(runner$submit_task_report)[[1]],
    list("example", NULL, NULL, "myinstance", changelog = NULL, timeout = 100))

  ## and via the api
  api <- build_api(runner, "path")

  res_api <- api$request("POST", "/v1/reports/example/run/",
                         list(timeout = 100, instance = "myinstance"))
  expect_equal(
    mockery::mock_args(runner$submit_task_report)[[2]],
    list("example", NULL, NULL, "myinstance", changelog = NULL, timeout = 100))
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
  expect_equal(res$changelog_types, c(scalar("internal"), scalar("public")))

  ## test through API
  api <- build_api(runner, "path")
  res <- api$request("GET", "/run-metadata")
  expect_equal(res$status, 200L)
  expect_equal(res$headers[["Content-Type"]], "application/json")
  expect_equal(res$body, as.character(endpoint$run()$body))
})

test_that("run-metadata pulls information from runner", {
  skip_if_no_redis()
  path <- orderly_git_example("minimal")
  runner <- orderly_runner(path, workers = 0)

  expect_equal(target_run_metadata(runner), list(
    name = NULL,
    instances_supported = scalar(FALSE),
    git_supported = scalar(TRUE),
    instances = list(
      "source" = character(0)
    ),
    changelog_types = NULL
  ))

  ## Example with all enabled
  path <- orderly_git_example("minimal")
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
    git_supported = scalar(TRUE),
    instances = list(
      "source" = c(scalar("production"), scalar("staging"))
    ),
    changelog_types = c(scalar("internal"), scalar("external"))
  ))
})

test_that("run-metadata can get config for multiple databases", {
  skip_if_no_redis()
  path <- orderly_git_example("minimal")
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
    git_supported = scalar(TRUE),
    instances = list(
      source = c(scalar("production"), scalar("staging")),
      annex = character(0)
    ),
    changelog_types = NULL
  ))

  path <- orderly_git_example("minimal")
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
    git_supported = scalar(TRUE),
    instances = list(
      source = c(scalar("production"), scalar("staging")),
      annex = c(scalar("annex1"), scalar("annex2"))
    ),
    changelog_types = NULL
  ))
})

test_that("run metadata can get name from config", {
  skip_if_no_redis()
  path <- orderly_git_example("minimal")
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
    git_supported = scalar(TRUE),
    instances = list(
      source = c(scalar("production"), scalar("staging"))
    ),
    changelog_types = NULL
  ))
})

test_that("run metadata returns git & db instances supported info", {
  skip_if_no_redis()
  path <- orderly_git_example("minimal")
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
           "    primary: TRUE",
           "    default_branch_only: TRUE",
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
    runner <- orderly_runner(path)
    metadata <- target_run_metadata(runner)
  })
  expect_equal(metadata, list(
    name = scalar("production"),
    instances_supported = scalar(FALSE),
    git_supported = scalar(FALSE),
    instances = NULL,
    changelog_types = NULL
  ))
})

test_that("bundle pack can pack basic bundle", {
  tmp <- tempfile(fileext = ".zip")
  writeBin(as.raw(0:255), tmp)

  mock_bundle_pack <- mockery::mock(list(path = tmp))

  with_mock("orderly::orderly_bundle_pack" = mock_bundle_pack, {
    endpoint <- endpoint_bundle_pack("root")
    res <- endpoint$run("name")
  })
  args <- mockery::mock_args(mock_bundle_pack)[[1]]
  expect_match(args[[1]], "[\\w/_]+", perl = TRUE)
  expect_equal(args[[2]], "name")
  expect_null(args[[3]])
  expect_equal(args[[4]], "root")
  expect_null(args[[5]])

  expect_equal(res$data, as.raw(0:255), check.attributes = FALSE)
  expect_equal(res$headers, list("Content-Disposition" = basename(tmp)))
  expect_equal(res$status_code, 200L)
})

test_that("bundle pack can pass parameters and instance", {
  tmp <- tempfile(fileext = ".zip")
  writeBin(as.raw(0:255), tmp)
  mock_bundle_pack <- mockery::mock(list(path = tmp))
  with_mock("orderly::orderly_bundle_pack" = mock_bundle_pack, {
    endpoint <- endpoint_bundle_pack("root")
    res <- endpoint$run("name", parameters = '{"a": 1}',
                        instance = "myinstance")
  })
  args <- mockery::mock_args(mock_bundle_pack)[[1]]
  expect_match(args[[1]], "[\\w/_]+", perl = TRUE)
  expect_equal(args[[2]], "name")
  expect_equal(args[[3]], list(a = 1))
  expect_equal(args[[4]], "root")
  expect_equal(args[[5]], "myinstance")

  expect_equal(res$data, as.raw(0:255), check.attributes = FALSE)
  expect_equal(res$headers, list("Content-Disposition" = basename(tmp)))
  expect_equal(res$status_code, 200L)
})

test_that("Can create and run bundles", {
  path <- orderly_prepare_orderly_example("demo")

  bundle_pack <- endpoint_bundle_pack(path)
  res <- bundle_pack$run("other", parameters = '{"nmin":0.5}')
  path_pack <- tempfile()
  writeBin(res$body, path_pack)
  expect_true(file.exists(path_pack))

  res <- orderly::orderly_bundle_run(path_pack, echo = FALSE)

  bundle_import <- endpoint_bundle_import(path)
  bundle_res <- bundle_import$run(readBin(res$path, "raw", 10e6))
  expect_equal(bundle_res$status_code, 200)
  expect_equal(
    orderly::orderly_list_archive(path),
    data.frame(name = "other", id = res$id, stringsAsFactors = FALSE))
})

test_that("api preroute calls runner check_timeout with rate limit", {
  path <- orderly_prepare_orderly_example("minimal")
  runner <- mock_runner()
  api <- build_api(runner, path)

  res <- api$request("GET", "/")
  expect_equal(res$status, 200L)
  mockery::expect_called(runner$check_timeout, 1)

  res <- api$request("GET", "/")
  expect_equal(res$status, 200L)
  ## Check_timeout is rate limited so not called 2nd time
  mockery::expect_called(runner$check_timeout, 1)
})

test_that("api runs backup on preroute", {
  skip_if_no_redis()
  path <- orderly_git_example("minimal")
  runner <- orderly_runner(path)
  api <- build_api(runner, path, backup_period = 1)
  db_backup <- orderly_path_db_backup(path, "orderly.sqlite")
  expect_true(file.exists(db_backup))
  dat_backup <- with_sqlite(db_backup, function(con) {
    DBI::dbReadTable(con, "report_version")
  })
  ## Nothing has been backed up yet
  expect_equal(nrow(dat_backup), 0)

  ## When report is run
  id <- orderly::orderly_run("example", root = path, echo = FALSE)
  orderly::orderly_commit(id, root = path)

  ## Call API endpoint to trigger backup from preroute
  Sys.sleep(1.2) ## ensure backup period has passed
  res <- api$request("GET", "/")
  expect_equal(res$status, 200L)

  ## Data exists in backup
  dat_backup <- with_sqlite(db_backup, function(con) {
    DBI::dbReadTable(con, "report_version")
  })
  expect_equal(nrow(dat_backup), 1)
  expect_equal(dat_backup$report, "example")
})

test_that("can get dependencies", {
  path <- orderly_prepare_orderly_example("demo")
  runner <- mock_runner(root = path)
  dependencies <- endpoint_dependencies(runner)
  res <- dependencies$run(name = "use_dependency",
                          id = NULL,
                          direction = "upstream",
                          use = "src")
  # Check data
  data <- res$data
  expect_equal(data$direction, scalar("upstream"))
  dep_tree <- data$dependency_tree
  expect_equal(dep_tree$name, scalar("use_dependency"))
  expect_equal(dep_tree$id, scalar("latest"))
  dependencies <- dep_tree$dependencies
  expect_equal(length(dependencies), 1)
  expect_equal(dependencies[[1]]$name, scalar("other"))
  expect_equal(dependencies[[1]]$id, scalar("latest"))
  expect_equal(length(dependencies[[1]]$dependencies), 0)
  # Check response
  expect_equal(res$status_code, 200)
  expected_json <- paste(sep = "",
                        "{\"status\":\"success\",\"errors\":null,\"data\":",
                          "{\"direction\":\"upstream\",\"dependency_tree\":",
                            "{\"name\":\"use_dependency\",\"id\":\"latest\",",
                               "\"out_of_date\":false,\"dependencies\":[",
                                 "{\"name\":\"other\",\"id\":\"latest\",",
                                    "\"out_of_date\":false,\"dependencies\":[]",
                                "}]}}}")
  expect_equal(res$body, expected_json)
})

test_that("endpoint_report_info can return info from report run", {
  path <- orderly_prepare_orderly_git_example()

  ## Run a report to retrieve info for
  append_lines('stop("some error")',
               file.path(path[["local"]], "src", "minimal", "script.R"))
  expect_error(orderly::orderly_run("minimal", root = path[["local"]],
                                    echo = FALSE), "some error")
  drafts <- orderly::orderly_list_drafts(root = path[["local"]],
                                         include_failed = TRUE)

  runner <- mock_runner(root = path[["local"]])
  endpoint <- endpoint_report_info(runner)
  info <- endpoint$run(id = drafts[["id"]], name = "minimal")
  expect_equal(info$status_code, 200)
  expect_equal(info$data$name, scalar("minimal"))
  expect_equal(info$data$id, scalar(drafts$id))
  expect_null(info$data$parameters)
  expect_equal(info$data$git$branch, scalar("master"))
  expect_match(info$data$git$ref, "[0-9a-f]{7}")
  expect_null(info$data$logfile)
  expect_equal(info$data$error$message, scalar("some error"))
  expect_true(length(info$data$error$trace) > 5)
  expect_match(as.character(
    info$data$error$trace[length(info$data$error$trace)]),
    "some error")
})

test_that("endpoint_report_info returns parameter info", {
  path <- orderly_prepare_orderly_example("demo")
  id <- orderly::orderly_run("other", parameters = list(nmin = 0.1),
                             root = path, echo = FALSE)

  runner <- mock_runner(root = path)
  endpoint <- endpoint_report_info(runner)
  info <- endpoint$run(id = id, name = "other")
  expect_equal(info$status_code, 200)
  expect_equal(info$data$name, scalar("other"))
  expect_equal(info$data$id, scalar(id))
  expect_equal(info$data$params, list(nmin = scalar(0.1)))
  expect_null(info$data$git)
  expect_null(info$data$logfile)
  expect_null(info$data$error)
})


test_that("can retrieve information about artefacts", {
  path <- orderly_prepare_orderly_example("demo")
  id <- orderly::orderly_run("other", parameters = list(nmin = 0.1),
                             root = path, echo = FALSE)
  orderly::orderly_commit(id, root = path)
  runner <- mock_runner(root = path)

  data <- target_report_version_artefact(runner, id)
  endpoint <- endpoint_report_version_artefact(runner)
  res <- endpoint$run(id = id)

  expect_true(res$validated)
  expect_equal(res$status_code, 200)
  expect_type(res$data, "list")
  ## No need to check the format of the data all over as that's
  ## duplicated by the schema check.  But here, check the first file
  ## is indeed first:
  expect_equal(res$data[[1]]$id, scalar(1L))
  expect_equal(res$data[[1]]$description, scalar("A summary table"))
  expect_equal(res$data[[1]]$files[[1]]$filename, scalar("summary.csv"))
})
