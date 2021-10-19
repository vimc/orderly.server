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

test_that("readlines_if_exists", {
  t <- tempfile()
  writeLines("sample text", t)
  expect_equal(readlines_if_exists(t), "sample text")
  test <- readlines_if_exists("missing/file", "it is missing")
  expect_equal(test, "it is missing")
})

test_that("throttle does not call functions very often", {
  f <- mockery::mock(1, 2)
  g <- throttle(f, 0.5)
  expect_equal(g(), 1)
  expect_null(g())
  mockery::expect_called(f, 1)
  Sys.sleep(0.6)
  expect_equal(g(), 2)
  mockery::expect_called(f, 2)
})

test_that("format_changelog", {
  expect_equal(format_changelog(list(type = "tst", message = "test")),
               "[tst] test")
  expect_error(format_changelog(list(type = "tst")),
               "'changelog$message' must be a scalar", fixed = TRUE)
  expect_null(format_changelog(NULL))
})

test_that("can recursively convert to scalar", {
  expect_equal(recursive_scalar(2), list(scalar(2)))
  expect_equal(recursive_scalar(c(1, 2)), list(scalar(1), scalar(2)))
  expect_equal(recursive_scalar(list(x = "foo",
                                     y = "bar")),
               list(x = scalar("foo"),
                    y = scalar("bar")))
  expect_equal(recursive_scalar(list(x = "foo",
                                     y = list(y1 = "bar1", y2 = "bar2"))),
               list(x = scalar("foo"),
                    y = list(y1 = scalar("bar1"), y2 = scalar("bar2"))))
  expect_null(recursive_scalar(NULL))
  expect_equal(recursive_scalar(list(x = "foo", y = NULL)),
               list(x = scalar("foo"), y = NULL))
  expect_equal(recursive_scalar(list(x = 1)),
               list(x = scalar(1)))
})

test_that("collector", {
  collection <- key_value_collector()
  expect_equal(collection$length(), 0)
  expect_equal(collection$get_all(), list())
  collection$add("test", "new item")
  expect_equal(collection$length(), 1)
  expect_equal(collection$get_all(), list(test = "new item"))
  collection$add("test", "item2")
  expect_equal(collection$length(), 1)
  expect_equal(collection$get_all(), list(test = c("new item", "item2")))
  collection$add("foo", c("A", "B"))
  expect_equal(collection$length(), 2)
  expect_equal(collection$get_all(), list(
    test = c("new item", "item2"),
    foo = c("A", "B")
  ))
  expect_equal(collection$get("foo"), c("A", "B"))
  expect_equal(collection$get("test"), c("new item", "item2"))
  expect_null(collection$get("bar"))
  expect_null(collection$get(NULL))
  expect_equal(collection$get(c("foo", "test")),
                              c("A", "B", "new item", "item2"))
})

test_that("ordered_map_to_list", {
  expect_equal(ordered_map_to_list(yaml_load("- a: 1\n- b: 2")),
               list(a = 1, b = 2))

  ## The yaml parser will catch this sort of thing
  expect_error(yaml_load("- a: 1\n- b: 2\n c: 3"))

  ## but if it came through it would be as
  d <- list(list(a = 1), list(b = 2, c = 3))
  expect_error(ordered_map_to_list(d),
               "Corrupt ordered map (this should never happen)",
               fixed = TRUE)
})
