context("api-workflows")

test_that("workflow can be validated", {
  path <- orderly_prepare_orderly_example("minimal")
  t <- tempfile()
  writeLines(jsonlite::toJSON(list(
    tasks = list(
      preprocess = list(
        ref = scalar("123"),
        instance = list(
          source = scalar("production")
        ),
        params = list(
          nmin = scalar(0.5),
          nmax = scalar(2)
        )
      ),
      process = list(
        ref = scalar("ba123"),
        instance = list(
          source = scalar("production")
        )
      ),
      postprocess = list(
        ref = scalar("2134"),
        instance = list(
          source = scalar("production")
        )
      )
    )
  )), t)

  validate_output <- list(
    missing_dependencies = list(
      preprocess = "",
      process = "",
      postprocess = ""
    )
  )
  mock_validate <- mockery::mock(validate_output, cycle = TRUE)
  with_mock("orderly.server:::workflow_validate" = mock_validate, {
    res <- target_workflow_validate(path, t)
  })
  mockery::expect_called(mock_validate, 1)
  expect_equal(res, validate_output)

  ## endpoint
  with_mock("orderly.server:::workflow_validate" = mock_validate, {
    endpoint <- endpoint_workflow_validate(path)
    res_endpoint <- endpoint$run(t)
  })
  expect_equal(res_endpoint$status_code, 200)
  expect_equal(res_endpoint$data, res)
  mockery::expect_called(mock_validate, 2)

  ## api
  with_mock("orderly.server:::workflow_validate" = mock_validate, {
    api <- build_api(mock_runner(), path)
    res_api <- api$request("POST", "/v1/workflow/validate/",
                           body = readLines(t))
  })
  expect_equal(res_api$status, 200L)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, as.character(res_endpoint$body))
  mockery::expect_called(mock_validate, 3)
})
