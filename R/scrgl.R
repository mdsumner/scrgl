#' Silicate methods
#'
#' @importFrom silicate sc_coord sc_vertex
#' @export sc_coord sc_vertex
#' @name silicate-mesh3d-methods
sc_vertex.mesh3d <- function(x, ..., prefer_triangles = TRUE){
  tibble::as_tibble(stats::setNames(as.data.frame(t(x[["vb"]])), c("x_", "y_", "z_", "h")))
}
#' @export
#' @name silicate-mesh3d-methods
sc_coord.mesh3d <- function(x, ..., prefer_triangles = TRUE) {
  pnames <- c("it", "ib")
  ## the order ensures the order after intersect
  if (!prefer_triangles) pnames <- rev(pnames)
  primitives <- intersect(pnames, names(x))[1L]
  tibble::as_tibble(stats::setNames(as.data.frame(t(x[["vb"]][, x[[primitives]]])), c("x_", "y_", "z_", "h")))
}


