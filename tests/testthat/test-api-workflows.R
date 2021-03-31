context("api-workflows")

test_that("workflow can be validated", {
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

  res <- target_workflow_validate("path", t)
  expect_equal(res,
               list(missing_dependencies = list(
                 preprocess = "",
                 process = "",
                 postprocess = ""
               )))

  ## endpoint
  endpoint <- endpoint_workflow_validate("path")
  res_endpoint <- endpoint$run(t)
  expect_equal(res_endpoint$status_code, 200)
  expect_equal(res_endpoint$data, res)

  ## api
  runner <- mock_runner()
  api <- build_api(runner, "path")
  res_api <- api$request("POST", "/v1/workflow/validate/",
                         body = readLines(t))
  expect_equal(res_api$status, 200L)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, as.character(res_endpoint$body))
})
