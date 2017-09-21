
is_directory <- function(x) {
  isTRUE(file.info(x, extra_cols = FALSE)$isdir)
}
