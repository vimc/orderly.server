workflow_missing_dependencies <- function(path, reports) {
  all_reports <- orderly::orderly_list(root = path)
  missing_deps <- lapply(names(reports), function(report) {
    if (!(report %in% all_reports)) {
      stop(sprintf("Report with name '%s' cannot be found.", report))
    }
    get_missing_dependencies(report, path, reports)
  })
  list(missing_dependencies = setNames(missing_deps, names(reports)))
}

get_missing_dependencies <- function(report, path, reports) {
  graph <- orderly::orderly_graph(report, root = path, direction = "upstream",
                                  use = "src", max_depth = 1)
  dependencies <- lapply(graph$root$children, function(vertex) {
    scalar(vertex$name)
  })
  deps <- dependencies[!(dependencies %in% names(reports))]
}
