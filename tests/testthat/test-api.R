context("api - endpoints")

test_that("index", {
  x <- endpoint_index(NULL)

  expected <- list(name = scalar("orderly.server"),
                    version = scalar("0.0.0"),
                    endpoints = c("comming", "soon"))
  expect_equal(x$target(), expected)

  api <- pkgapi::pkgapi$new()$handle(x)
  res <- api$request("GET", "/")
  expect_equal(res$status, 200L)
  expect_equal(res$headers[["Content-Type"]], "application/json")
  expect_equal(res$body, as.character(x$run()$body))
})

test_that("rebuild", {

  server <- start_test_server()
  on.exit(server$stop())

  r <- httr::POST(server$api_url("/v1/reports/rebuild/"))
  dat <- content(r)
  expect_equal(dat$status, "success")
  expect_null(dat$data)
  expect_equal(dat$errors, list())
})
