main <- function(args = commandArgs(TRUE)) {
  args <- main_args(args)
  do.call("server", args)
}

main_args <- function(args) {
  opts <- list(
    optparse::make_option("--port",
                          help = "Port to run on",
                          type = "integer",
                          default = "8321"),
    optparse::make_option("--host",
                          help = "IP address owned by this server",
                          default = "0.0.0.0",
                          type = "character"),
    optparse::make_option("--no-ref",
                          help = "Prevent git reference switching",
                          type = "logical",
                          default = FALSE,
                          action = "store_true",
                          dest = "no_ref"),
    optparse::make_option("--go-signal",
                          help = "Path for go signal (within path)",
                          default = NULL,
                          type = "character",
                          dest = "go_signal"))
  parser <- optparse::OptionParser(
    option_list = opts,
    usage = "%prog [options] <path>")
  res <- optparse::parse_args(parser, args, positional_arguments = 1L)

  list(path = res$args[[1L]],
       port = res$options$port,
       host = res$options$host,
       allow_ref = !res$options$no_ref,
       go_signal = res$options$go_signal)
}

write_script <- function(path) {
  if (!isTRUE(file.info(path, extra_cols = FALSE)$isdir)) {
    stop("'path' must be a directory")
  }
  code <- c("#!/usr/bin/env Rscript", "orderly.server:::main()")
  path_bin <- file.path(path, "orderly.server")
  writeLines(code, path_bin)
  Sys.chmod(path_bin, "755")
  invisible(path_bin)
}
