main <- function(args = commandArgs(TRUE)) {
  args <- main_args(args)
  do.call("server", args)
}

main_args <- function(args) {
  doc <- "Usage:
  orderly.server [options] <path>

Options:
  --port=PORT       Port to run on [default: 8321]
  --host=HOST       IP address owned by this server [default: 0.0.0.0]
  --no-ref          Prevent git reference switching
  --go-signal=PATH  Relative path for go signal"
  res <- docopt::docopt(doc, args)

  list(path = res[["path"]],
       port = as.integer(res[["port"]]),
       host = res[["host"]],
       allow_ref = !res[["no-ref"]],
       go_signal = res[["go-signal"]])
}

write_script <- function(path, code) {
  if (!isTRUE(file.info(path, extra_cols = FALSE)$isdir)) {
    stop("'path' must be a directory")
  }
  code <- c("#!/usr/bin/env Rscript", code)
  path_bin <- file.path(path, "orderly.server")
  writeLines(code, path_bin)
  Sys.chmod(path_bin, "755")
  invisible(path_bin)
}
