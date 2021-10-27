context("api-workflows")

test_that("can identify missing dependencies of a workflow", {
  testthat::skip_on_cran()
  skip_on_windows()
  skip_if_no_redis()
  path <- orderly_git_example("minimal")
  runner <- orderly_runner(path)
  t <- tempfile()
  ref <- git_ref_to_sha("HEAD", root = path)
  writeLines(jsonlite::toJSON(list(
    reports = list(
      list(
        name = scalar("preprocess"),
        instance = scalar("production"),
        params = list(
          nmin = scalar(0.5),
          nmax = scalar(2)
        )
      ),
      list(
        name = scalar("process"),
        instance = scalar("production")
      ),
      list(
        name = scalar("postprocess"),
        instance = scalar("production")
      )
    ),
    ref = scalar(ref)
  )), t)

  output <- list(
    missing_dependencies = list(
      preprocess = "",
      process = "",
      postprocess = ""
    )
  )
  mock_missing_dependencies <- mockery::mock(output, cycle = TRUE)
  with_mock("orderly.server:::workflow_missing_dependencies" =
              mock_missing_dependencies, {
    res <- target_workflow_missing_dependencies(runner, t)
  })
  mockery::expect_called(mock_missing_dependencies, 1)
  expect_equal(res, output)

  ## endpoint
  with_mock("orderly.server:::workflow_missing_dependencies" =
              mock_missing_dependencies, {
    endpoint <- endpoint_workflow_missing_dependencies(runner)
    res_endpoint <- endpoint$run(t)
  })
  expect_equal(res_endpoint$status_code, 200)
  expect_equal(res_endpoint$data, res)
  mockery::expect_called(mock_missing_dependencies, 2)

  ## api
  with_mock("orderly.server:::workflow_missing_dependencies" =
              mock_missing_dependencies, {
    api <- build_api(runner, path)
    res_api <- api$request("POST", "/v1/workflow/missing-dependencies/",
                           body = readLines(t))
  })
  expect_equal(res_api$status, 200L)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, as.character(res_endpoint$body))
  mockery::expect_called(mock_missing_dependencies, 3)
})

test_that("workflow job can be submitted", {
  path <- orderly_git_example("depends", testing = TRUE)
  runner <- mock_runner(submit_workflow = list(
    workflow_key = "123",
    reports = c("report1_key", "report2_key")))
  reports <- list(
    list(
      name = scalar("report1")
    ),
    list(
      name = scalar("report2")
    )
  )
  ref <- scalar("ref123")
  changelog <- list(
    message = scalar("changelog 1"),
    type = scalar("internal")
  )
  body <- jsonlite::toJSON(list(
    reports = reports,
    ref = ref,
    changelog = changelog
  ))

  res <- list(
    workflow_key = scalar("123"),
    reports = c(scalar("report1_key"), scalar("report2_key")))
  expect_equal(target_workflow_run(runner, body), res)
  args <- mockery::mock_args(runner$submit_workflow)
  expect_length(args, 1)
  expect_equal(args[[1]][[1]], list(
    list(name = "report1"),
    list(name = "report2")))
  expect_equal(args[[1]][[2]], "ref123")
  expect_equal(args[[1]][[3]],  "[internal] changelog 1")

  ## endpoint
  endpoint <- endpoint_workflow_run(runner)
  output <- endpoint$run(body)
  expect_equal(output$status_code, 200)
  expect_equal(output$data, res)
  args <- mockery::mock_args(runner$submit_workflow)
  expect_length(args, 2)
  expect_equal(args[[2]][[1]], list(
    list(name = "report1"),
    list(name = "report2")))
  expect_equal(args[[2]][[2]], "ref123")
  expect_equal(args[[2]][[3]],  "[internal] changelog 1")

  ## api
  api <- build_api(runner, path)
  res_api <- api$request("POST", "/v1/workflow/run/",
                         body = body)
  expect_equal(res_api$status, 200)
  output <- jsonlite::fromJSON(res_api$body)
  expect_equal(output$status, "success")
  expect_null(output$errors)
  expect_equal(output$data$workflow_key, "123")
  expect_equal(output$data$reports, c("report1_key", "report2_key"))
  args <- mockery::mock_args(runner$submit_workflow)
  expect_length(args, 3)
  expect_equal(args[[3]][[1]], list(
    list(name = "report1"),
    list(name = "report2")))
  expect_equal(args[[3]][[2]], "ref123")
  expect_equal(args[[3]][[3]],  "[internal] changelog 1")
})

test_that("additional parameters are passed to task run", {
  testthat::skip_on_cran()
  skip_on_windows()
  skip_if_no_redis()
  path <- orderly_git_example("demo")
  runner <- orderly_runner(path)
  ref <- git_ref_to_sha("HEAD", root = path)
  reports <- list(
    list(
      name = scalar("other"),
      params = list(
        nmin = 0.5
      )
    ),
    list(
      name = scalar("minimal")
    )
  )
  changelog <- list(
    message = scalar("changelog 1"),
    type = scalar("internal")
  )
  body <- jsonlite::toJSON(list(
    reports = reports,
    ref = scalar(ref),
    changelog = changelog
  ))

  api <- build_api(runner, path)
  res_api <- api$request("POST", "/v1/workflow/run/",
                         body = body)
  keys <- jsonlite::fromJSON(res_api$body)$data$reports
  task_ids <- vcapply(keys, function(key) get_task_id_key(runner, key))
  result_1 <- runner$queue$task_wait(task_ids[[1]])
  result_2 <- runner$queue$task_wait(task_ids[[2]])

  data_1 <- runner$queue$task_data(task_ids[1])
  expect_equal(data_1$expr$key, keys[1])
  expect_equal(data_1$expr$name, "other")
  expect_equal(data_1$expr$parameters, list(nmin = 0.5))
  expect_equal(data_1$expr$ref, ref)
  expect_equal(data_1$expr$changelog, "[internal] changelog 1")
  expect_equal(data_1$expr$poll, 0.1)
  res_1 <- runner$status(keys[1])
  expect_equal(res_1$status, "success")

  data_2 <- runner$queue$task_data(task_ids[2])
  expect_equal(data_2$expr$key, keys[2])
  expect_equal(data_2$expr$name, "minimal")
  expect_null(data_2$expr$parameters)
  expect_equal(data_2$expr$ref, ref)
  expect_equal(data_2$expr$changelog,  "[internal] changelog 1")
  expect_equal(data_2$expr$poll, 0.1)
  res_2 <- runner$status(keys[2])
  expect_equal(res_2$status, "success")
})

test_that("single report workflow can be run", {
  path <- orderly_git_example("depends", testing = TRUE)
  runner <- mock_runner(submit_workflow = list(
    workflow_key = "123",
    reports = "report1_key"))
  reports <- list(
    list(
      name = scalar("report1")
    )
  )
  ref <- scalar("ref123")
  changelog <- list(
    message = scalar("changelog 1"),
    type = scalar("internal")
  )
  body <- jsonlite::toJSON(list(
    reports = reports,
    ref = ref,
    changelog = changelog
  ))

  res <- list(
    workflow_key = scalar("123"),
    reports = "report1_key")

  endpoint <- endpoint_workflow_run(runner)
  output <- endpoint$run(body)
  expect_equal(output$status_code, 200)
  expect_equal(output$data, res)
  args <- mockery::mock_args(runner$submit_workflow)
  expect_length(args, 1)
  expect_equal(args[[1]][[1]], list(
    list(name = "report1")))
  expect_equal(args[[1]][[2]], "ref123")
  expect_equal(args[[1]][[3]],  "[internal] changelog 1")
})

test_that("report can be included in a workflow twice", {
  testthat::skip_on_cran()
  skip_on_windows()
  skip_if_no_redis()
  path <- orderly_git_example("minimal")
  runner <- orderly_runner(path)
  ref <- git_ref_to_sha("HEAD", root = path)
  reports <- list(
    list(
      name = scalar("example")
    ),
    list(
      name = scalar("example")
    )
  )
  body <- jsonlite::toJSON(list(
    reports = reports,
    ref = scalar(ref)
  ))

  api <- build_api(runner, path)
  res_api <- api$request("POST", "/v1/workflow/run/",
                         body = body)
  keys <- jsonlite::fromJSON(res_api$body)$data$reports
  task_ids <- vcapply(keys, function(key) get_task_id_key(runner, key))
  expect_length(task_ids, 2)

  data_1 <- runner$queue$task_data(task_ids[1])
  expect_equal(data_1$expr$key, keys[1])
  expect_equal(data_1$expr$name, "example")
  expect_equal(data_1$expr$ref, ref)
  expect_equal(data_1$expr$poll, 0.1)

  data_2 <- runner$queue$task_data(task_ids[2])
  expect_equal(data_2$expr$key, keys[2])
  expect_equal(data_2$expr$name, "example")
  expect_equal(data_2$expr$ref, ref)
  expect_equal(data_2$expr$poll, 0.1)
})

test_that("workflow status", {
  workflow_key <- "workflow-key"
  workflow_status <- list(workflow_key = workflow_key,
                          status = "queued",
                          reports = list(
                            list(
                              key = "key1",
                              status = "queued",
                              version = NULL,
                              output = NULL,
                              queue = list()
                            )
                          ))

  runner <- mock_runner(key, workflow_status = workflow_status)

  res <- target_workflow_status(runner, workflow_key)
  expect_equal(
    res,
    list(workflow_key = scalar(workflow_key),
         status = scalar("queued"),
         reports = list(
           list(
             key = scalar("key1"),
             status = scalar("queued"),
             version = NULL,
             output = NULL,
             queue = list()
           )
         )))
  mockery::expect_called(runner$workflow_status, 1)
  expect_equal(mockery::mock_args(runner$workflow_status)[[1]],
               list(workflow_key, FALSE))

  ## endpoint
  endpoint <- endpoint_workflow_status(runner)
  res_endpoint <- endpoint$run(workflow_key)
  expect_equal(res_endpoint$status_code, 200)
  expect_equal(res_endpoint$content_type, "application/json")
  expect_equal(res_endpoint$data, res)
  mockery::expect_called(runner$workflow_status, 2)
  expect_equal(mockery::mock_args(runner$workflow_status)[[2]],
               list(workflow_key, FALSE))

  ## api
  api <- build_api(runner, "path")
  res_api <- api$request("GET",
                         sprintf("/v1/workflow/%s/status/", workflow_key))
  expect_equal(res_api$status, 200L)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, as.character(res_endpoint$body))
  mockery::expect_called(runner$workflow_status, 3)
  expect_equal(mockery::mock_args(runner$workflow_status)[[3]],
               list(workflow_key, FALSE))
})

test_that("workflow status - queued", {
  workflow_key <- "workflow-key"
  workflow_status <- list(workflow_key = workflow_key,
                          status = "queued",
                          reports = list(
                            list(
                              key = "key-2",
                              status = "queued",
                              version = NULL,
                              output = NULL,
                              queue = list(
                                list(
                                  key = "key-1",
                                  status = "running",
                                  name = "minimal",
                                  version = "20210310-123928-fef89bc7"
                                )))))

  runner <- mock_runner(key, workflow_status = workflow_status)

  res <- target_workflow_status(runner, workflow_key)
  expect_equal(
    res,
    list(workflow_key = scalar(workflow_key),
         status = scalar("queued"),
         reports = list(
           list(
             key = scalar("key-2"),
             status = scalar("queued"),
             version = NULL,
             output = NULL,
             queue = list(
               list(
                 key = scalar("key-1"),
                 status = scalar("running"),
                 name = scalar("minimal"),
                 version = scalar("20210310-123928-fef89bc7")
               )
             )
           )
         )))
  mockery::expect_called(runner$workflow_status, 1)
  expect_equal(mockery::mock_args(runner$workflow_status)[[1]],
               list(workflow_key, FALSE))
})

test_that("git commits endpoint returns valid refs for workflow running", {
  ## Test for vimc-4822
  testthat::skip_on_cran()
  skip_on_windows()
  skip_if_no_redis()
  path <- orderly_prepare_orderly_git_example()
  runner <- orderly_runner(path[["local"]])

  commits <- git_commits("master", root = path[["local"]])
  expect_equal(nrow(commits), 1)

  expect_no_error(res <- runner$submit_workflow(list(list(name = "global")),
                                                commits$id))
  task_id <- get_task_id_key(runner, res$reports)
  result <- runner$queue$task_wait(task_id)

  commits <- git_commits("master", root = path[["local"]])
  ## Now 2 commits as we have fetched from remote
  expect_equal(nrow(commits), 2)
  expect_no_error(runner$submit_workflow(list(list(name = "global")),
                                         commits$id[1]))
})


test_that("mrc-2626: workflow can be queued on new branch", {
  testthat::skip_on_cran()
  skip_on_windows()
  skip_if_no_redis()
  path <- orderly_prepare_orderly_example(name = "depends", testing = TRUE,
                                          git = TRUE)
  runner <- orderly_runner(path)

  ## Make a change to origin to represent someone updating remote
  ## e.g. merging a new PR
  ## Setup another report on a branch to check works with different ref
  master <- git_checkout_branch("example", root = path, create = TRUE)
  new_report <- file.path(path, "src/depend5")
  dir.create(new_report)
  file.copy(list.files(file.path(path, "src/depend2"), full.names = TRUE),
            file.path(path, "src/depend5"), recursive = TRUE)
  git_run(c("add", "."), root = path, check = TRUE)
  gert::git_commit("Add depend5 report", repo = path,
                   author = "Test User <test.user@example.com>")
  hash <- git_run(c("rev-parse", "--short", "HEAD"), root = path, check = TRUE)
  git_checkout_branch(master, root = path)

  ## Pull to represent someone hitting "refresh git" button in OW
  target_git_pull(path)

  ## Get commits
  commits <- target_git_commits(path, "example")
  expect_equal(nrow(commits), 1)

  multiple_deps <- list(
    list(
      name = "example"
    ),
    list(
      name = "depend5"
    ),
    list(
      name = "depend2"
    )
  )
  res <- runner$submit_workflow(multiple_deps, ref = commits$id)
  testthat::try_again(5, {
    Sys.sleep(0.5)
    tasks <- runner$queue$task_list()
    expect_length(tasks, 3)
  })
  expect_equal(names(res), c("workflow_key", "reports"))
  expect_true(!is.null(res$workflow_key))
  redis_key <- workflow_redis_key(runner$queue$queue_id, res$workflow_key)
  expect_setequal(runner$con$SMEMBERS(redis_key), res$reports)
  expect_length(res$reports, 3)
  task_ids <- vcapply(res$reports, function(id) get_task_id_key(runner, id))
  expect_setequal(tasks, task_ids)
})

test_that("workflow submit response lists reports in same order as request", {
  skip_if_no_redis()
  skip_on_covr()
  path <- orderly_prepare_orderly_example(name = "depends", testing = TRUE,
                                          git = TRUE)
  runner <- orderly_runner(path)
  reports <- list(
    list(
      name = "depend3"
    ),
    list(
      name = "depend2"
    ),
    list(
      name = "example"
    ),
    list(
      name = "depend3"
    ),
    list(
      name = "depend2"
    ),
    list(
      name = "example"
    )
  )
  res <- runner$submit_workflow(reports)
  testthat::try_again(30, {
    Sys.sleep(0.5)
    status <- runner$workflow_status(res$workflow_key)
    result <- lapply(status$reports, "[[", "status")
    expect_true(all(result == "success"))
  })

  status_keys <- vcapply(status$reports, "[[", "key")
  expect_setequal(status_keys, res$reports)
  ## If `orderly_info` can find reports when we specify their name and id
  ## then we know response has been returned in same order as input
  check_same <- function(report, key) {
    status <- status$reports[status_keys == key]
    expect_length(status, 1)
    expect_no_error(orderly::orderly_info(id = status[[1]]$version,
                                          name = report$name,
                                          root = path))
  }
  Map(check_same, reports, res$reports)
})
