main <- function(args = commandArgs(TRUE)) {
  args <- main_args(args)
  do.call("server", args)
}

main_args <- function(args) {
  doc <- "Usage:
  orderly.server [options] <path>

Options:
  --port=PORT                    Port to run on [default: 8321]
  --host=HOST                    IP address owned by this server [default: 0.0.0.0]
  --no-ref                       Prevent git reference switching
  --go-signal=PATH               Relative path for go signal
  --queue-id=ID                  rrq ID
  --workers=WORKERS              Number of workers to spawn [default: 0]
  --backup-period=BACKUP_PERIOD  How frequently should backup be run, 0 or negative for no backup [default: 600]"
  res <- docopt_parse(doc, args)

  backup_period <- as.integer(res[["backup_period"]])
  if (backup_period <= 0) {
    backup_period <- NULL
  }

  list(path = res[["path"]],
       port = as.integer(res[["port"]]),
       host = res[["host"]],
       allow_ref = !res[["no_ref"]],
       go_signal = res[["go_signal"]],
       queue_id = res[["queue_id"]],
       workers = as.integer(res[["workers"]]),
       backup_period = backup_period)
}

main_worker_args <- function(args = commandArgs(TRUE)) {
  usage <- "Usage:
orderly_worker [<queue_id>]"
  dat <- docopt_parse(usage, args)
  list(queue_id = dat$queue_id)
}

main_worker <- function(args = commandArgs(TRUE)) {
  # nocov start
  args <- main_worker_args(args)
  rrq::rrq_worker(orderly_queue_id(args$queue_id, TRUE),
                  heartbeat_period = 10)
  # nocov end
}

docopt_parse <- function(doc, args) {
  dat <- docopt::docopt(doc, args)
  names(dat) <- gsub("-", "_", names(dat), fixed = TRUE)
  dat
}

write_script <- function(path, code, versioned = FALSE) {
  if (!isTRUE(file.info(path, extra_cols = FALSE)$isdir)) {
    stop("'path' must be a directory")
  }
  if (versioned) {
    rscript <- file.path(R.home(), "bin", "Rscript")
  } else {
    rscript <- "/usr/bin/env Rscript"
  }
  code <- c(sprintf("#!%s", rscript), code)
  path_bin <- file.path(path, "orderly.server")
  writeLines(code, path_bin)
  Sys.chmod(path_bin, "755")
  invisible(path_bin)
}
