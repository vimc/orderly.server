main <- function(args = commandArgs(TRUE)) {
  opts <- list(
    optparse::make_option("--port",
                          help = "Port to run on",
                          type = "integer",
                          default = "8123"),
    optparse::make_option("--host",
                          help = "IP address owned by this server",
                          default = "0.0.0.0",
                          type = "character"))
  parser <- optparse::OptionParser(
    option_list = opts,
    usage = "%prog [options] <path>")
  res <- optparse::parse_args(parser, args, positional_arguments = 1L)

  path <- res$args[[1L]]
  port <- res$options$port
  host <- res$options$host
  server(path, port, host)
}

is_directory <- function(x) {
  isTRUE(file.info(x, extra_cols = FALSE)$isdir)
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
