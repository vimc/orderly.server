context("util")

test_that("is_directory", {
  path <- tempfile()
  expect_false(is_directory(path))
  file.create(path)
  expect_false(is_directory(path))
  expect_true(is_directory(tempdir()))
})


test_that("wait_while", {
  expect_lt(wait_while(function() FALSE, 0, 0), 1)
  expect_error(wait_while(function() TRUE, 0, 0),
               "Timeout reached")
  expect_error(wait_while(function() TRUE, 0.05, 0.01),
               "Timeout reached")
})


test_that("protect", {
  f <- function() {
    if (x < 0) {
      stop("negative x")
    } else {
      x
    }
  }
  g <- protect(f)
  x <- 1
  expect_equal(g(), 1)
  x <- -1
  expect_null(g())
})

test_that("periodic", {
  e <- new.env(parent = emptyenv())
  e$x <- 1

  skip_on_windows() # timing on windows is a pain
  skip_on_cran() # gc may cause occasional failures here
  gc() # avoid slow collections during this test
  f <- function() {
    e$x <- e$x + 1
  }
  g <- periodic(f, 0.1)
  g()
  expect_equal(e$x, 1)
  Sys.sleep(0.2)
  g()
  expect_equal(e$x, 2)
  g()
  expect_equal(e$x, 2)
})

test_that("yaml load", {
  yml <- c("a:", "  - 1", "  - 2", "b:", "    3", "c: 4")
  expect_equal(yaml_load(yml), list(a = 1:2, b = 3, c = 4))
  text <- c("a: 1", "a: 2")
  expect_error(yaml_load(text))
})

## I'm not sure there's anything super safe to run this with in
## general...
test_that("sys_which", {
  prog <- "a-path-that-does-not-exist"
  expect_error(sys_which(prog),
               "Did not find 'a-path-that-does-not-exist'")
})

test_that("calculating age uses seconds", {
  now <- Sys.time()
  times <- c(as.integer(now - 10000), as.integer(now + 10000))
  ## Stub Sys.time for easy of testing
  mockery::stub(calculate_age, "Sys.time", now)
  expect_equal(calculate_age(times), c(10000, -10000))
})

test_that("first_dirname gets the first dir part of the filename", {
  expect_equal(
    first_dirname(c("test/file/name.txt", "test", ".", "testing/file.txt")),
    c("test", "test", ".", "testing"))
})

test_that("copy failure", {
  path1 <- tempfile()
  path2 <- tempfile()
  writeLines("a", path1)
  writeLines("b", path2)
  on.exit(file.remove(path1, path2))
  expect_error(file_copy(path1, path2, overwrite = FALSE),
               "Error copying files")
  expect_equal(readLines(path2), "b")
})
