workflow_validate <- function(path, tasks) {
  reports <- orderly::orderly_list(root = path)
  missing_deps <- lapply(names(tasks), function(task) {
    if (!(task %in% reports)) {
      stop(sprintf("Task with name '%s' cannot be found.", task))
    }
    get_missing_dependencies(task, path, tasks)
  })
  list(missing_dependencies = setNames(missing_deps, names(tasks)))
}

get_missing_dependencies <- function(task, path, tasks) {
  graph <- orderly::orderly_graph(task, root = path, direction = "upstream",
                                  use = "src", max_depth = 1)
  dependencies <- lapply(graph$root$children, function(vertex) {
    scalar(vertex$name)
  })
  ## TODO: consider search query dependencies i.e.
  ## latest(parameters:iso3 = iso3)
  ## these should probably only be considered if their parameter matches between
  ## dependency and this task
  deps <- dependencies[!(dependencies %in% names(tasks))]
}
