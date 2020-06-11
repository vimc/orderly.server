#!/usr/bin/env Rscript

if (isFALSE(require("pkgload"))) {
  install.packages("pkgload")
}
if (isFALSE(require("testthat"))) {
  install.packages("testthat")
}
pkgload::load_all()
path <- orderly_prepare_orderly_example("interactive")

testthat::expect_false(file.exists(file.path(path, "orderly.sqlite")))
runner <- orderly_runner(path)
testthat::expect_true(file.exists(file.path(path, "orderly.sqlite")))
name <- "interactive"
key <- runner$queue(name)

testthat::expect_equal(runner$status(key),
             list(key = key,
                  status = "queued",
                  id = NA_character_,
                  output = list(stdout = character(), stderr = NULL)))

tmp <- runner$poll()
testthat::expect_equal(tmp, structure("create", key = key))
id <- wait_for_id(runner, key)

st <- runner$status(key)
testthat::expect_is(st$id, "character")
testthat::expect_equal(st$status, "running")

dat <- runner$status(key, TRUE)
testthat::expect_equal(names(dat$output), c("stderr", "stdout"))
testthat::expect_match(dat$output$stderr, paste0("\\[ id +\\]  ", id),
             all = FALSE)

wait_for_path(file.path(path, "draft", name, id, "started"))
writeLines("continue", file.path(path, "draft", name, id, "resume"))
wait_while_running(runner)

dat2 <- runner$status(key, TRUE)
testthat::expect_equal(dat2$status, "success")

testthat::expect_equal(dat2$output$stdout[seq_along(dat$output$stdout)],
             dat$output$stdout)
testthat::expect_equal(dat2$output$stderr[seq_along(dat$output$stderr)],
             dat$output$stderr)
testthat::expect_gt(length(dat2$output$stderr), length(dat$output$stderr))
testthat::expect_equal(dat$output$stdout, character())
