#' @include repr.R
NULL

Shape <- new_class(
  "Shape",
  properties = list(
    dims = S7::class_numeric
  ),
  constructor = function(dims) {
    new_object(
      S7::S7_object(),
      dims = as.integer(dims)
    )
  },
  validator = function(self) {
    if (!is.integer(self@dims)) {
      stop("dims must be an integer vector")
    }

    dims = self@dims
    if (any(dims[!is.na(dims)] < 0L)) {
      stop("Dimensions must be >= 0")
    }
  }
)

method(`==`, list(Shape, Shape)) <- function(e1, e2) {
  identical(e1@dims, e2@dims)
}

method(repr, Shape) <- function(x) {
  dims = x@dims
  dims[is.na(dims)] = "?"
  paste0(dims, collapse = "x")
}

shape <- new_generic(
  "shape",
  "x",
  function(x, ...) {
    S7::S7_dispatch()
  }
)

method(shape, Shape) <- function(x) {
  x@dims
}

method(shape, S7::class_any) <- function(x) {
  dim(x)
}
