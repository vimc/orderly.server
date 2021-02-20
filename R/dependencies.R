get_dependencies <- function(path, name, id=NULL, direction=NULL, propagate=NULL, max_depth=100, show_all=NULL, use=NULL) {
  print(paste("Getting from orderly", name))
  
  
  if (length(name) != 1) {
    stop(sprintf("DEBUG name must be a scalar:  %s", name), call. = FALSE)
  }
  
  if (length(path) != 1) {
    stop(sprintf("DEBUG path  must be a scalar: %s", path), call. = FALSE)
  }
  
  if (length(id) != 1) {
    stop(sprintf("DEBUG id must be a scalar: %s", id), call. = FALSE)
  }
  
  if (length(direction) != 1) {
    stop(sprintf("DEBUG direction must be a scalar: %s", direction), call. = FALSE)
  }
  
  if (length(propagate) != 1) {
    stop(sprintf("DEBUG propagate must be a scalar: %s", propagate), call. = FALSE)
  }
  
  if (length(max_depth) != 1) {
    stop(sprintf("DEBUG max_depth must be a scalar: %s", max_depth), call. = FALSE)
  }
  
  if (length(show_all) != 1) {
    stop(sprintf("DEBUG show_all must be a scalar: %s", show_all), call. = FALSE)
  }
  
  if (length(use) != 1) {
    stop(sprintf("DEBUG use must be a scalar: %s", use), call. = FALSE)
  }
  
  report_tree <- orderly::orderly_graph(name=name, id=id, 
                                       root=path, locate=FALSE, 
                                       direction=direction, propagate=propagate, 
                                       max_depth=max_depth, show_all=show_all, 
                                       use=use)
  print(paste("Got from orderly", name))
  
  root <- report_tree$root
  dep <- vertex_to_dep(root)
  
  list(
    direction = scalar(report_tree$get_direction()),
    dependency_tree = dep
  )
}

vertex_to_dep <- function(vertex) {
  list(
    name = scalar(vertex$name),
    id = scalar(vertex$id),
    out_of_date = scalar(vertex$out_of_date),
    dependencies = vertices_to_deps(vertex$children)
  )
}

vertices_to_deps <- function(vertices) {
  lapply(vertices, vertex_to_dep)
}
