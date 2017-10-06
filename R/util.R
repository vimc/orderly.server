is_directory <- function(x) {
  isTRUE(file.info(x, extra_cols = FALSE)$isdir)
}

as_logical <- function(x, name = deparse(substitute(x))) {
  switch(tolower(x),
         "true" = TRUE,
         "false" = FALSE,
         stop(sprintf("Invalid input for '%s'", name)))
}

to_json <- function(x, ...) {
  jsonlite::toJSON(x, auto_unbox = TRUE, ...)
}

time_diff_secs <- function(t, t0) {
  dt <- t - t0
  units(dt) <- "secs"
  as.integer(dt)
}

prepare_orderly_git_demo <- function() {
  git_run <- orderly:::git_run
  path1 <- orderly:::unzip_git_demo()
  path2 <- tempfile()
  git_run(c("clone", "--", path1, path2), check = TRUE)
  writeLines("new", file.path(path1, "new"))
  git_run(c("add", "."), path1)
  git_run(c("commit", "-m", "orderly"), path1)
  file.copy(file.path(path1, "source.sqlite"),
            file.path(path2, "source.sqlite"))
  c(origin = path1, local = path2)
}
