get_dependencies <- function(path, name, id=NULL, direction=NULL, propagate=NULL, max_depth=NULL, show_all=NULL, use=NULL) {
  report_tree <- orderly::orderly_graph(name=scalar(name), id=scalar(id), 
                                       root=scalar(path), locate=scalar(FALSE), 
                                       direction=scalar(direction), propagate=scalar(propagate), 
                                       max_depth=scalar(max_depth), show_all=scalar(show_all), 
                                       use=scalar(use))
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
