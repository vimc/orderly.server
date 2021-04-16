##' Get missing dependencies for list of reports.
##'
##' Report A has a missing dependency if, report A has a "depends" block in
##' its `orderly.yml` which depends on latest version (or latest with some
##' search term) of report B and report B does not occur in the list of reports.
##'
##' @param path Path to orderly root.
##' @param reports List of reports to get missing dependencies for,
##'   this must be at a minimum a list of lists each item containing "name"
##'   property of the report name. It can have additional data too but this
##'   is ignored for checking dependencies.
##'
##' @return List with key report name and value is any missing dependencies.
##'
##' @keywords internal
workflow_missing_dependencies <- function(path, reports) {
  all_reports <- orderly::orderly_list(root = path)
  report_names <- lapply(reports, function(report) report$name)
  missing_deps <- lapply(reports, function(report) {
    if (!(report$name %in% all_reports)) {
      stop(sprintf("Report with name '%s' cannot be found.", report$name))
    }
    get_missing_dependencies(report$name, path, report_names)
  })
  list(missing_dependencies = setNames(missing_deps, report_names))
}

get_missing_dependencies <- function(report_name, path, report_names) {
  graph <- orderly::orderly_graph(report_name, root = path,
                                  direction = "upstream",
                                  use = "src", max_depth = 1)
  dependencies <- lapply(graph$root$children, function(vertex) {
    scalar(vertex$name)
  })
  deps <- dependencies[!(dependencies %in% report_names)]
}
