get_dependencies <- function(path, name, id, direction, propagate, max_depth, show_all, use) {
  print(paste("Getting from orderly", name))
  
  
  #if (length(name) != 1) {
  #  stop("DEBUG name must be a scalar:  %s", name)
  #}
  
  #if (length(path) != 1) {
  #  stop("DEBUG path  must be a scalar: %s", path)
  #}
  
  #if (length(id) != 1) {
  #  stop("DEBUG id must be a scalar: %s", id)
  #}
  
  #if (length(direction) != 1) {
  #  stop("DEBUG direction must be a scalar: %s", direction)
#  }
  
  #if (length(propagate) != 1) {
  #  stop("DEBUG propagate must be a scalar: %s", propagate)
  #}
  
  #if (length(max_depth) != 1) {
  #  stop("DEBUG max_depth must be a scalar: %s", max_depth)
  #}
  
  #if (length(show_all) != 1) {
  #  stop("DEBUG show_all must be a scalar: %s", show_all)
  #}
  
  #if (length(use) != 1) {
  #  stop("DEBUG use must be a scalar: %s", use)
  #}
  
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
