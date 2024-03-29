context("logging")

test_that("Can log verbosely", {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  logger <- porcelain::porcelain_logger("trace", path = tmp)
  runner <- mock_runner()
  api <- build_api(runner, logger = logger)
  res <- api$request("GET", "/")
  lapply(readLines(tmp), jsonlite::fromJSON)

  dat <- jsonlite::stream_in(file(tmp), verbose = FALSE)
  expect_equal(nrow(dat), 4)
  expect_equal(dat$logger, rep("orderly.server", 4))
})
