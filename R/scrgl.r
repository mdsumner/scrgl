## currently we can build PATH by defining
# sc_coord, sc_object and sc_path

## we need PRIMITIVE to be able to build from those inputs like
## rgl models, rbgm, igraph, TopoJSON, maps

library(rangl)
data("minimal_mesh", package = "scsf")
x <- plot(rangl(minimal_mesh))
r <- plot(rangl(raster(matrix(1:12, 3))))
library(rbgm)
bgm <- bgmfile(bgmfiles::bgmfiles()[3])
class(bgm) <- "bgm"  ## pretend we did this


## any of these might return NULL
sc_quad(r)
sc_quad(x)
#sc_segment(r)
#sc_segment(x)

sc_triangle(r)
sc_triangle(x)

##
sc_object(r)
sc_object(x)
sc_vertex(r)
sc_vertex(x)


sc_ver
sc_vertex_index <- function(x, ...) {
  tx <- tibble::as_tibble(t(x))
  dplyr::mutate(setNames(tx, paste0(".vertex", seq_len(ncol(tx)) - 1L)), object_ = sc::sc_rand(1L))
}

sc_quad <- function(x, ...) {
  if (is.null(x[["ib"]])) return(NULL)
  sc_vertex_index(x$ib, ...)
}
sc_triangle <- function(x, ...) {
 UseMethod("sc_triangle")
}
sc_triangle.bgm <- function(x, ...) {
  NULL
}
sc_triangle.mesh3d <- function(x, ...) {
  if (is.null(x[["it"]])) return(NULL)
  sc_vertex_index(x$it, ...)
}

#sc_segment ## there's no mesh type for segments?
## the material properties
sc_object.mesh3d <- function(x, ...) {
  tibble::tibble(object_ = 1)
}
sc_object.bgm <- function(x, ...) {
  rename(x[["boxes"]], .object_ = .bx0)
}

#' Return the unique vertices, with ID. (sc_coord returns all instances, with no ID)
sc_vertex <- function(x, ...) {
  UseMethod("sc_vertex")
}
sc_vertex.mesh3d <- function(x, ...) {
  dplyr::mutate(stats::setNames(tibble::as_tibble(t(x$vb)), c("X", "Y", "Z", "W")))
}
sc_vertex.bgm <- function(x, ...) {
  dplyr::rename(x[["vertices"]], .vertex_ = .vx0)
}




PRIMITIVE.oneday.default <- function(x, ...) {
  ## get the main stuff
  library(tibble)
  library(dplyr)
  o <- sc_object(x)
  o[["object_"]] <- sc::sc_rand(nrow(o))
  b <- sc_triangle(x, ids = o[["object_"]])

  v <- sc_vertex(x)

  key_col <- "vertex_"
  maindata <- unjoin::unjoin_(dplyr::mutate(v, path_ = rep(b$path_, b$ncoords_)), names(v), key_col = key_col)
  id <- sc_rand(n = nrow(maindata[["data"]]))
  v <- dplyr::mutate(maindata[[key_col]], vertex_ = id[maindata[[key_col]][[key_col]]])
  bXv <- dplyr::mutate(maindata[["data"]], vertex_ = id[maindata[["data"]][[key_col]]])
  #v[[key_col]] <- bXv[[key_col]] <- NULL
  join_ramp <-  tabnames <- c("object", "path",  "path_link_vertex", "vertex")
  structure(list(object = o, path = b, vertex = v, path_link_vertex = bXv),
            class = c("PATH", "sc"),
            join_ramp = join_ramp)
}
