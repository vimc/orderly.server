get_dependencies <- function(path, name, id,
                             direction, propagate, max_depth, show_all, use) {
  report_tree <- orderly::orderly_graph(name = name, id = id,
                                       root = path, locate = FALSE,
                                       direction = direction,
                                       propagate = propagate,
                                       max_depth = max_depth,
                                       show_all = show_all,
                                       use = use)
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
