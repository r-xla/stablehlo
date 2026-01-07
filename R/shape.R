#' @include repr.R
NULL

#' @title Shape
#' @description
#' Represents the shape of a tensor.
#' @param dims (`integer()`)
#' @return `Shape`
#' @export
Shape <- function(dims = integer()) {
  dims <- as.integer(dims)

  if (any(dims[!is.na(dims)] < 0L)) {
    cli_abort("Dimensions must be >= 0")
  }

  structure(
    list(dims = dims),
    class = "Shape"
  )
}

#' @export
`==.Shape` <- function(e1, e2) {
  identical(e1$dims, e2$dims)
}

#' @export
repr.Shape <- function(x, ...) {
  dims <- x$dims
  dims[is.na(dims)] <- "?"
  paste0(dims, collapse = "x")
}

#' @export
print.Shape <- function(x, ...) {
  cat("<Shape: ", repr(x), ">\n", sep = "")
  invisible(x)
}

#' @export
#' @method shape Shape
shape.Shape <- function(x, ...) {
  x$dims
}

#' @export
#' @method dtype TensorConstant
dtype.TensorConstant <- function(x, ...) {
  x$type$dtype
}

#' @export
#' @method dtype FuncValue
dtype.FuncValue <- function(x, ...) {
  dtype(x$value_type)
}
