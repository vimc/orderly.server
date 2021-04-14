context("api-workflows")

test_that("workflow can be validated", {
  path <- orderly_prepare_orderly_example("minimal")
  t <- tempfile()
  writeLines(jsonlite::toJSON(list(
    reports = list(
      preprocess = list(
        instance = list(
          source = scalar("production")
        ),
        params = list(
          nmin = scalar(0.5),
          nmax = scalar(2)
        )
      ),
      process = list(
        instance = list(
          source = scalar("production")
        )
      ),
      postprocess = list(
        instance = list(
          source = scalar("production")
        )
      )
    ),
    ref = scalar("123")
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
    res <- target_workflow_missing_dependencies(path, t)
  })
  mockery::expect_called(mock_missing_dependencies, 1)
  expect_equal(res, output)

  ## endpoint
  with_mock("orderly.server:::workflow_missing_dependencies" =
              mock_missing_dependencies, {
    endpoint <- endpoint_workflow_missing_dependencies(path)
    res_endpoint <- endpoint$run(t)
  })
  expect_equal(res_endpoint$status_code, 200)
  expect_equal(res_endpoint$data, res)
  mockery::expect_called(mock_missing_dependencies, 2)

  ## api
  with_mock("orderly.server:::workflow_missing_dependencies" =
              mock_missing_dependencies, {
    api <- build_api(mock_runner(), path)
    res_api <- api$request("POST", "/v1/workflow/missing-dependencies/",
                           body = readLines(t))
  })
  expect_equal(res_api$status, 200L)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, as.character(res_endpoint$body))
  mockery::expect_called(mock_missing_dependencies, 3)
})
