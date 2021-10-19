orderly_upstream_dependencies <- function(reports, root = NULL, locate = TRUE,
                                          ref = NULL) {
  assert_character(reports)
  if (!is.null(ref)) {
    assert_scalar_character(ref)
  }
  config <- orderly::orderly_config(root, locate)
  setNames(lapply(reports, report_dependencies, ref, config), reports)
}

report_dependencies <- function(name, ref, config) {
  path <- file.path("src", name, "orderly.yml")
  lines <- git_show(path, ref = ref, root = config$root)
  depends <- yaml_load(lines$output)$depends
  if (is.null(depends)) {
    return(NULL)
  }
  ## Deal with yaml weirdness:
  if (is.null(names(depends))) {
    depends <- ordered_map_to_list(depends)
  }
  unique(names(depends))
}

git_show <- function(path, ref = NULL, root = NULL) {
  if (is.null(ref)) {
    ref <- "HEAD"
  }
  path <- sprintf("%s:%s", ref, path)
  git_run(c("show", path), root = root, check = TRUE)
}

ordered_map_to_list <- function(x) {
  ## This should not happen, but this is what would happen if we had
  ## a corrupted ordered map.  I think that the yaml parsers will
  ## fix that for us though.
  if (!all(lengths(x) == 1L)) {
    stop("Corrupt ordered map (this should never happen)")
  }
  stopifnot(vlapply(x, function(el) !is.null(names(el))))
  set_names(lapply(x, function(x) x[[1]]),
            vcapply(x, names))
}

vlapply <- function(X, FUN, ...) { # nolint
  vapply(X, FUN, logical(1), ...)
}

set_names <- function(x, nms) {
  names(x) <- nms
  x
}
