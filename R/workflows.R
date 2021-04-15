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

get_missing_dependencies <- function(task, path, tasks) {
  dependencies <- get_report_dependencies(task, path)
  deps <- dependencies[!(dependencies %in% names(tasks))]
}

get_report_dependencies <- function(report, path) {
  graph <- orderly::orderly_graph(report, root = path, direction = "upstream",
                                  use = "src", max_depth = 1)
  lapply(graph$root$children, function(vertex) {
    scalar(vertex$name)
  })
}


## Build a representation like
## reportA = NA
## reportB = reportC
## reportC = NA
## reportD = c(reportA, reportB)
## means A & C have no dependencies, B depends on C, D depends on A & B
build_dependencies_graph <- function(path, reports) {
  ## TODO: When building these dependencies need to make sure we are
  ## respecting git ref - should git ref be top level param?
  get_present_dependencies <- function(report) {
    dependencies <- unique(unlist(get_report_dependencies(report, path)))
    present_deps <- names(reports)[names(reports) %in% dependencies]
    if (length(present_deps) == 0) {
      present_deps <- NA
    }
    present_deps
  }
  deps_graph <- lapply(names(reports), get_present_dependencies)
  names(deps_graph) <- names(reports)
  deps_graph
}

build_workflow <- function(path, reports, ref, changelog, key_report_id, poll) {
  dependencies_graph <- build_dependencies_graph(path, reports)
  order <- topological_sort(dependencies_graph)
  build_item <- function(report) {
    report_details <- reports[[report]]
    prepare_task(key_report_id, path, report, report_details$parameters,
                 ref, report_details$instance, changelog, poll,
                 depends_on = dependencies_graph[[report]])
  }
  workflow <- lapply(order, build_item)
  names(workflow) <- order
  workflow
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

prepare_task <- function(key_report_id, root, name, parameters = NULL,
                         ref = NULL, instance = NULL, changelog = NULL,
                         poll = 0.1, depends_on = NULL) {
  key <- ids::adjective_animal()
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
    key = key,
    depends_on = depends_on
  )
}
