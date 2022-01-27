##' Get workflow summary for this set of reports
##'
##' This includes info on execution order of reports and any dependencies
##' between them and details of any missing dependencies.
##'
##' Report A has a missing dependency if, report A has a "depends" block in
##' its `orderly.yml` which depends on latest version (or latest with some
##' search term) of report B and report B does not occur in the list of reports.
##'
##' @param path Path to orderly root.
##' @param reports List of reports to get workflow summary for,
##'   this must be at a minimum a list of lists each item containing "name"
##'   property of the report name. It can have additional data too but this
##'   is ignored for checking dependencies.
##'
##' @return List containing reports in the order they will be
##'   queued including info about any dependencies between them. Also
##'   a list with key report name and value is any missing dependencies
##'   for that report.
##'
##' @keywords internal
workflow_summary <- function(path, reports, ref = NULL) {
  all_reports <- vcapply(reports, function(report) report$name,
                         USE.NAMES = FALSE)
  report_names <- unique(all_reports)
  dependencies <- orderly_upstream_dependencies(report_names, root = path,
                                                ref = ref)
  missing <- missing_dependencies(report_names, dependencies)

  list(
    reports = construct_workflow(reports, report_names, dependencies),
    ref = ref,
    missing_dependencies = missing[all_reports]
  )
}

##' Get missing dependencies for list of reports.
##'
##' Report A has a missing dependency if, report A has a "depends" block in
##' its `orderly.yml` which depends on latest version (or latest with some
##' search term) of report B and report B does not occur in the list of reports.
##'
##' @param path Path to orderly root.
##' @param reports Report names to get missing dependencies for
##' @param dependencies Names list of report to its dependencies
##'
##' @return List with key report name and value is any missing dependencies.
##'
##' @keywords internal
missing_dependencies <- function(report_names, dependencies) {
  missing_deps <- lapply(report_names, function(report) {
    report_deps <- dependencies[[report]]
    if (is.null(report_deps) || all(report_deps %in% report_names)) {
      return(list())
    }
    as.list(report_deps[!(report_deps %in% report_names)])
  })
  stats::setNames(missing_deps, report_names)
}

## Build a representation like
## > - reportA = NA
## > - reportB = reportC
## > - reportC = NA
## > - reportD = c(reportA, reportB)
## means A & C have no dependencies, B depends on C, D depends on A & B
workflow_dependencies <- function(report_names, dependencies) {

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
  report_names <- unique(vcapply(reports, function(report) report$name))
  dependencies <- orderly_upstream_dependencies(report_names, root = root,
                                                ref = ref)
  ## We want to return in order which this workflow run was requested
  ## preserve this now for ordering the result before return
  counter <- 1
  for (index in seq_along(reports)) {
    reports[[index]]$original_order <- counter
    counter <- counter + 1
  }
  construct_workflow(reports, report_names, dependencies)
}

construct_workflow <- function(reports, report_names, dependencies) {
  dependencies_graph <- workflow_dependencies(report_names, dependencies)
  order <- topological_sort(dependencies_graph)
  all_report_names <- vcapply(reports, function(report) report$name)

  build_item <- function(report_name) {
    ## There may be multiple reports due to be run with this name
    report_details <- reports[report_name == all_report_names]
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

topological_sort <- function(graph) {
  report_names <- names(graph)
  graph_sorted <- NULL
  explored <- rep(FALSE, length(graph))
  deps <- lapply(seq_along(graph),
                 function(i) which(report_names %in% graph[[i]]))

  stack <- 1

  while (any(!explored)) {
    if (length(stack) > 1 && head(stack, n = 1) == tail(stack, n = 1)) {
      stack <- rev(stack)
      node <- stack[1]
      nodes <- node
      stack <- stack[-1]
      while (length(stack) > 1) {
        children <- which(report_names %in% graph[[node]])
        index <- seq_along(children)
        stack <- stack[-index]
        node <- children[1]
        nodes <- c(nodes, node)
      }

      f <- function(i) {
        sprintf("  %s: depends on %s",
                names(graph)[[i]], paste(graph[[i]], collapse = ", "))
      }

      detail <- paste(vcapply(nodes, f), collapse = "\n")
      stop(sprintf("A cyclic dependency detected for %s:\n%s",
                   paste(names(graph)[nodes], collapse = ", "), detail))
    }

    i <- stack[1]
    if (!explored[i]) {
      if (length(deps[[i]]) == 0 || all(explored[deps[[i]]])) {
        graph_sorted <- c(graph_sorted, i)
        explored[i] <- TRUE
        stack <- stack[-1]
      } else {
        stack <- c(which(report_names %in% graph[[i]]), stack)
      }
    } else {
      stack <- stack[-1]
    }
    if (length(stack) == 0) {
      stack <- which(!explored)[1]
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

## Has format matching WorkflowSummaryResponse schema
## but we need to be careful with tagging items as scalar so it
## serializes properly
serialize_workflow_summary <- function(workflow) {

  serialize_report <- function(single_report) {
    item <- list(
      name = scalar(single_report$name)
    )
    item$instance <- scalar(single_report$instance)
    item$params <- recursive_scalar(single_report$params)
    item$depends_on <- single_report$depends_on
    item
  }

  list(
    reports = lapply(workflow$reports, serialize_report),
    ref = scalar(workflow$ref),
    missing_dependencies = recursive_scalar(workflow$missing_dependencies)
  )
}
