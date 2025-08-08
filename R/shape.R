#' @include repr.R
NULL

Shape <- new_class(
  "Shape",
  properties = list(
    dims = S7::class_numeric
  ),
  validator = function(self) {
    stopifnot(isTRUE(all.equal(self@dims, as.integer(self@dims))))
    dims = as.integer(self@dims)
    if (any(dims[!is.na(dims)] <= 0L)) {
      stop("Dimensions must be positive")
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
