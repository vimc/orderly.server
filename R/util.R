is_directory <- function(x) {
  isTRUE(file.info(x, extra_cols = FALSE)$isdir)
}


`%||%` <- function(a, b) { # nolint
  if (is.null(a)) b else a
}


wait_while <- function(continue, timeout = 10, poll = 0.02) {
  t_start <- Sys.time()
  t_quit <- t_start + timeout
  now <- t_start
  while (continue()) {
    Sys.sleep(poll)
    now <- Sys.time()
    if (now > t_quit) {
      stop("Timeout reached")
    }
  }
  now - t_start
}

scalar <- function(x) {
  jsonlite::unbox(x)
}

recursive_scalar <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  lapply(x, function(item) {
    if (length(item) > 1 || is.list(item)) {
      out <- recursive_scalar(item)
    } else {
      out <- scalar(item)
    }
    out
  })
}

vcapply <- function(X, FUN, ...) { # nolint
  vapply(X, FUN, character(1), ...)
}

vnapply <- function(X, FUN, ...) { # nolint
  vapply(X, FUN, numeric(1), ...)
}

protect <- function(fun) {
  fun <- match.fun(fun)
  function() {
    tryCatch(fun(), error = function(e) NULL)
  }
}

periodic <- function(fun, period) {
  fun <- match.fun(fun)
  force(period)
  env <- new.env(parent = emptyenv())
  env$last <- Sys.time()
  function() {
    now <- Sys.time()
    if (now > env$last + period) {
      fun()
      env$last <- now
    }
  }
}

yaml_load <- function(string) {
  ## More restrictive true/false handling.  Only accept if it maps to
  ## full (true|yes) / (false|no):
  handlers <- list(
    "bool#yes" = function(x)
      if (tolower(x) %in% c("true", "yes")) TRUE else x,
    "bool#no" = function(x)
      if (tolower(x) %in% c("false", "no")) FALSE else x)
  yaml::yaml.load(string, handlers = handlers)
}

sys_which <- function(name) {
  path <- Sys.which(name)
  if (!nzchar(path)) {
    stop(sprintf("Did not find '%s'", name), call. = FALSE)
  }
  unname(path)
}

system3 <- function(command, args) {
  res <- suppressWarnings(system2(command, args, stdout = TRUE, stderr = TRUE))
  code <- attr(res, "status") %||% 0
  attr(res, "status") <- NULL
  list(success = code == 0,
       code = code,
       output = res)
}

calculate_age <- function(times) {
  as.integer(as.numeric(Sys.time(), "secs")) - times
}

convert_unix_to_iso_time <- function(times) {
  strftime(as.POSIXct(times, origin = "1970-01-01", tz = "UTC"))
}

first_dirname <- function(paths) {
  first_dir <- function(path) {
    if (basename(path) == path) {
      dir <- path
    } else {
      dir <- first_dirname(dirname(path))
    }
    dir
  }
  vcapply(paths, first_dir, USE.NAMES = FALSE)
}

path_draft <- function(root) {
  file.path(root, "draft")
}

path_archive <- function(root) {
  file.path(root, "archive")
}

readlines_if_exists <- function(path, missing = NULL) {
  if (file.exists(path)) {
    readLines(path)
  } else {
    missing
  }
}

file_copy <- function(..., overwrite = TRUE) {
  ok <- file.copy(..., overwrite = overwrite)
  if (any(!ok)) {
    stop("Error copying files")
  }
  ok
}

dir_create <- function(path) {
  dir.create(path, FALSE, TRUE)
}

# Run a command with a rate limiter - this is used to throttle the
# timeout check.
throttle <- function(f, every) {
  last <- Sys.time() - every
  function() {
    now <- Sys.time()
    if (now - every > last) {
      last <<- now
      f()
    }
  }
}

format_changelog <- function(changelog) {
  if (is.null(changelog)) {
    return(changelog)
  }
  assert_scalar_character(changelog$type)
  assert_scalar_character(changelog$message)
  sprintf("[%s] %s", changelog$type, changelog$message)
}

key_value_collector <- function(init = list()) {
  env <- new.env(parent = emptyenv())
  env$res <- init
  add <- function(key, value) {
    env$res[[key]] <- c(env$res[[key]], value)
  }
  get <- function(key) {
    env$res[[key]]
  }
  list(add = add,
       length = function(x) length(env$res),
       get_all = function() env$res,
       get = function(keys) unlist(lapply(keys, get), recursive = FALSE))
}

squote <- function(x) {
  sprintf("'%s'", x)
}
