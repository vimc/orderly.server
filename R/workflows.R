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
  list(missing_dependencies = stats::setNames(missing_deps, report_names))
}

get_missing_dependencies <- function(report_name, path, report_names) {
  dependencies <- get_report_dependencies(report_name, path)
  deps <- dependencies[!(dependencies %in% report_names)]
}

get_report_dependencies <- function(report_name, path) {
  graph <- orderly::orderly_graph(report_name, root = path,
                                  direction = "upstream",
                                  use = "src", max_depth = 1)
  lapply(graph$root$children, function(vertex) {
    scalar(vertex$name)
  })
}


## Build a representation like
## - reportA = NA                     # nolint
## - reportB = reportC                # nolint
## - reportC = NA                     # nolint
## - reportD = c(reportA, reportB)    # nolint
## means A & C have no dependencies, B depends on C, D depends on A & B
build_dependencies_graph <- function(path, reports) {
  report_names <- vcapply(reports, function(report) report$name)
  get_present_dependencies <- function(report) {
    dependencies <- unique(unlist(get_report_dependencies(report$name, path)))
    present_deps <- report_names[report_names %in% dependencies]
    if (length(present_deps) == 0) {
      present_deps <- NA
    }
    present_deps
  }
  deps_graph <- lapply(reports, get_present_dependencies)
  names(deps_graph) <- report_names
  deps_graph
}

build_workflow <- function(root, alternative_root, reports, ref, changelog,
                           key_report_id, poll) {
  dependencies_graph <- build_dependencies_graph(alternative_root, reports)
  order <- topological_sort(dependencies_graph)
  ## We want to return in order which this workflow run was requested
  ## preserve this now for ordering the result before return
  counter <- 1
  for (index in seq_along(reports)) {
    reports[[index]]$original_order <- counter
    counter <- counter + 1
  }
  report_names <- vcapply(reports, function(report) report$name)
  build_item <- function(report_name) {
    ## There may be multiple reports due to be run with this name
    report_details <- reports[report_name == report_names]
    lapply(report_details, function(report_detail) {
      prepare_task(key_report_id, root, report_name,
                   report_detail$original_order,
                   report_detail$params, ref,
                   report_detail$instance, changelog, poll,
                   depends_on = dependencies_graph[[report_name]])
    })
  }
  workflow <- lapply(order, build_item)
  ## Workflow is list of lists of task details - collapse to
  ## list of task details
  do.call(c, workflow)
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
        ## Note that this is not going to give the right answer here
        ## but it might still be useful (dim_x -> dim(x), initial_x ->
        ## initial(x) etc.)  Could swap these around with
        ## RESERVED_PREFIX perhaps.
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

prepare_task <- function(key_report_id, root, name, original_order,
                         parameters = NULL, ref = NULL, instance = NULL,
                         changelog = NULL, poll = 0.1, depends_on = NULL) {
  key <- ids::adjective_animal()
  ## We should in future support multiple instances, at the
  ## moment this only supports 1 instance of type "source" see VIMC-4561
  instance <- instance$source
  expr <- quote(
    orderly.server:::runner_run(key_report_id, key, root, name,   # nolint
                                parameters, instance, ref,
                                changelog = changelog, poll = poll))
  if (anyNA(depends_on)) {
    depends_on <- NULL
  }
  list(
    expr = expr,
    envir = environment(),
    name = name,
    key = key,
    depends_on = depends_on,
    original_order = original_order
  )
}
