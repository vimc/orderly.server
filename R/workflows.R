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
workflow_missing_dependencies <- function(path, reports, ref = NULL) {
  report_names <- vcapply(reports, function(report) report$name,
                          USE.NAMES = FALSE)
  dependencies <- orderly::orderly_dependencies(report_names, root = path,
                                                ref = ref,
                                                direction = "upstream")
  missing_deps <- lapply(report_names, function(report) {
    report_deps <- dependencies[[report]]
    if (is.null(report_deps)) {
      return(list())
    }
    recursive_scalar(
      report_deps[!(report_deps %in% report_names)])
  })
  list(missing_dependencies = stats::setNames(missing_deps, report_names))
}

## Build a representation like
## > - reportA = NA
## > - reportB = reportC
## > - reportC = NA
## > - reportD = c(reportA, reportB)
## means A & C have no dependencies, B depends on C, D depends on A & B
build_dependencies_graph <- function(path, reports, ref = NULL) {
  report_names <- unique(vcapply(reports, function(report) report$name))
  dependencies <- orderly::orderly_dependencies(report_names, root = path,
                                                ref = ref,
                                                direction = "upstream")
  get_present_dependencies <- function(report) {
    present_deps <- report_names[report_names %in% dependencies[[report]]]
    if (length(present_deps) == 0) {
      present_deps <- NA
    }
    present_deps
  }
  deps_graph <- lapply(report_names, get_present_dependencies)
  names(deps_graph) <- report_names
  deps_graph
}

build_workflow <- function(root, reports, ref) {
  dependencies_graph <- build_dependencies_graph(root, reports, ref)
  order <- topological_sort(dependencies_graph)
  ## We want to return in order which this workflow run was requested
  ## preserve this now for ordering the result before return
  counter <- 1
  for (index in seq_along(reports)) {
    reports[[index]]$original_order <- counter
    counter <- counter + 1
  }
  report_names <- vcapply(reports, "[[", "name")
  build_item <- function(report_name) {
    ## There may be multiple reports due to be run with this name
    report_details <- reports[report_name == report_names]
    lapply(report_details, function(report_detail) {
      deps <- dependencies_graph[[report_name]]
      if (length(deps) == 0 || all(is.na(deps))) {
        deps <- NULL
      }
      report_detail$depends_on <- deps
      report_detail
    })
  }
  workflow <- lapply(order, build_item)
  unlist(workflow, recursive = FALSE, use.names = FALSE)
}

## This algorithm comes from here:
## http://blog.jupo.org/2012/04/06/topological-sorting-acyclic-directed-graphs/
## and assumes that the graph is expressed as a *named* list.  The
## daughters of an element are its dependencies.
topological_sort <- function(graph) {
  m <- matrix(FALSE, length(graph), length(graph))
  for (i in seq_along(graph)) {
    m[, i] <- unname(names(graph) %in% graph[[i]])
  }
  pending <- rep(TRUE, length(graph))
  graph_sorted <- integer(0)
  while (any(pending)) {
    i <- which(pending)[colSums(m[, pending, drop = FALSE]) == 0]
    if (length(i) > 0L) {
      graph_sorted <- c(graph_sorted, i)
      pending[i] <- FALSE
      m[i, ] <- FALSE
    } else {
      f <- function(i) {
        sprintf("  %s: depends on %s",
                names(graph)[[i]], paste(err[m[pending, i]], collapse = ", "))
      }
      err <- names(graph)[pending]
      detail <- paste(vcapply(which(pending), f), collapse = "\n")
      stop(sprintf(
        "A cyclic dependency detected for %s:\n%s",
        paste(names(graph)[pending], collapse = ", "),
        detail), call. = FALSE)
    }
  }
  names(graph)[graph_sorted]
}

workflow_combine_status <- function(report_status) {
  if (all(report_status == "success")) {
    workflow_status <- "success"
  } else if (any(report_status == "running")) {
    workflow_status <- "running"
  } else if (any(report_status == "cancelled")) {
    workflow_status <- "cancelled"
  } else if (all(report_status %in% c("queued", "deferred"))) {
    workflow_status <- "queued"
  } else {
    workflow_status <- "error"
  }
  workflow_status
}
