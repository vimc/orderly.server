context("logging")

test_that("Can log verbosely", {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  logger <- make_logger("trace", tmp)
  runner <- mock_runner()
  api <- build_api(runner, "path", logger = logger)
  res <- api$request("GET", "/")
  lapply(readLines(tmp), jsonlite::fromJSON)

  dat <- jsonlite::stream_in(file(tmp), verbose = FALSE)
  expect_equal(nrow(dat), 4)
  expect_equal(dat$logger, rep("orderly.server", 4))
})
