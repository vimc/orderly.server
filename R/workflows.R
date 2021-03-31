workflow_validate <- function(path, tasks) {
  missing_deps <- lapply(tasks, function(task) "")
  list(missing_dependencies = setNames(missing_deps, names(tasks)))
}
