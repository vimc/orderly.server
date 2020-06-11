context("main")

test_that("defaults", {
  expect_equal(
    main_args("path"),
    list(path = "path", port = 8321, host = "0.0.0.0", allow_ref = TRUE,
         go_signal = NULL))
})


test_that("set port", {
  expect_equal(
    main_args(c("path", "--port", "8888")),
    list(path = "path", port = 8888, host = "0.0.0.0", allow_ref = TRUE,
         go_signal = NULL))
})


test_that("set host", {
  expect_equal(
    main_args(c("path", "--host", "127.0.0.1")),
    list(path = "path", port = 8321, host = "127.0.0.1", allow_ref = TRUE,
         go_signal = NULL))
})


test_that("prevent reference switch", {
  expect_equal(
    main_args(c("path", "--no-ref")),
    list(path = "path", port = 8321, host = "0.0.0.0", allow_ref = FALSE,
         go_signal = NULL))
})


test_that("Set go signal", {
  expect_equal(
    main_args(c("path", "--go-signal", "somewhere")),
    list(path = "path", port = 8321, host = "0.0.0.0", allow_ref = TRUE,
         go_signal = "somewhere"))
})


test_that("write script", {
  p <- tempfile()
  expect_error(write_script(p), "'path' must be a directory")
  dir.create(p)
  res <- write_script(p, "orderly.server:::main()")
  expect_equal(basename(res), "orderly.server")
  expect_true(file.exists(res))
})


test_that("write script produces sensible script", {
  path <- tempfile()
  dir.create(path, FALSE, TRUE)
  bin <- write_script(
    path,
    readLines(system.file("script", package = "orderly", mustWork = TRUE)))
  expect_equal(basename(bin), "orderly.server")
  expect_true(file.exists(bin))
  expect_equal(readLines(bin)[[1]], "#!/usr/bin/env Rscript")
})


test_that("write script can be versioned", {
  path <- tempfile()
  dir.create(path, FALSE, TRUE)
  bin <- write_script(
    path,
    readLines(system.file("script", package = "orderly", mustWork = TRUE)),
    TRUE)
  expect_match(readLines(bin)[[1]], R.home(), fixed = TRUE)
})


test_that("pass arguments to server", {
  mock_server <- mockery::mock(NULL)
  with_mock("orderly.server:::server" = mock_server,
            main("path"))
  expect_equal(
    mockery::mock_args(mock_server)[[1]],
    main_args("path"))
})
