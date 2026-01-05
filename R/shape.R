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
    class = "stablehlo_Shape"
  )
}

#' @export
`==.stablehlo_Shape` <- function(e1, e2) {
  identical(e1$dims, e2$dims)
}

#' @export
repr.stablehlo_Shape <- function(x, ...) {
  dims <- x$dims
  dims[is.na(dims)] <- "?"
  paste0(dims, collapse = "x")
}

#' @export
#' @method shape stablehlo_Shape
shape.stablehlo_Shape <- function(x, ...) {
  x$dims
}

#' @export
#' @method dtype stablehlo_TensorConstant
dtype.stablehlo_TensorConstant <- function(x, ...) {
  x$type$dtype
}

#' @export
#' @method dtype stablehlo_FuncValue
dtype.stablehlo_FuncValue <- function(x, ...) {
  dtype(x$value_type)
}
