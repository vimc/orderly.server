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
        instance = list(
          source = scalar("production")
        ),
        params = list(
          nmin = scalar(0.5),
          nmax = scalar(2)
        )
      ),
      list(
        name = scalar("process"),
        instance = list(
          source = scalar("production")
        )
      ),
      list(
        name = scalar("postprocess"),
        instance = list(
          source = scalar("production")
        )
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
  expect_equal(args[[1]][[3]], list(
    message = "changelog 1",
    type = "internal"
  ))

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
  expect_equal(args[[2]][[3]], list(
    message = "changelog 1",
    type = "internal"
  ))

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
  expect_equal(args[[3]][[3]], list(
    message = "changelog 1",
    type = "internal"
  ))
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
      instance = list(
        source = scalar("production")
      ),
      params = list(
        nmin = 0.5
      )
    ),
    list(
      name = scalar("minimal"),
      instance = list(
        source = scalar("science")
      )
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

  data_1 <- runner$queue$task_data(task_ids[1])
  expect_equal(data_1$expr$key, keys[1])
  expect_equal(data_1$expr$name, "other")
  expect_equal(data_1$expr$parameters, list(nmin = 0.5))
  expect_equal(data_1$expr$instance, "production")
  expect_equal(data_1$expr$ref, ref)
  expect_equal(data_1$expr$changelog, list(
    message = "changelog 1",
    type = "internal"
  ))
  expect_equal(data_1$expr$poll, 0.1)

  data_2 <- runner$queue$task_data(task_ids[2])
  expect_equal(data_2$expr$key, keys[2])
  expect_equal(data_2$expr$name, "minimal")
  expect_null(data_2$expr$parameters)
  expect_equal(data_2$expr$instance, "science")
  expect_equal(data_2$expr$ref, ref)
  expect_equal(data_2$expr$changelog, list(
    message = "changelog 1",
    type = "internal"
  ))
  expect_equal(data_2$expr$poll, 0.1)
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
  expect_equal(args[[1]][[3]], list(
    message = "changelog 1",
    type = "internal"
  ))
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
