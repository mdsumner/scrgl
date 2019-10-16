# PRIMITIVE.mesh3d <- function(x, ...) {
#   stopifnot(x$primitivetype == "triangle")
#   if ("ib" %in% names(x)) {
#     warning("object has quad primitives as well as triangles,\n only the triangles will be carried through")
#   }
#   v <- setNames(tibble::as_tibble(t(x$vb[1:3, ])), c("x_", "y_", "z_"))
#   v$vertex_ <- sc::sc_rand(nrow(v))
#   object <- tibble::tibble(object_ = sc::sc_rand(1L))
#   primitive02 <- tibble::tibble(triangle_ = sc::sc_rand(ncol(x$it)),
#                                 object_ = rep(object$object_[1L], ncol(x$it)))
#
#   primitive02_link_vertex <- tibble::tibble(vertex_ = v$vertex_[x$it], triangle_ = primitive02$triangle_[rep(seq(ncol(x$it)),
#                                                                                                              each = 3)])
#   structure(list(object = object,
#                  primitive02 = primitive02,
#                  primitive02_link_vertex = primitive02_link_vertex,
#                  vertex = v), class = c("PRIMITIVE", "sc"))
# }
