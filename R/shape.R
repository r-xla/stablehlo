#' @include repr.R
NULL

#' @title Shape
#' @description
#' Represents the shape of a tensor: the size of each axis, `NA` where a size
#' is only known at run time.
#'
#' A `Shape` *is* its integer vector, with a class attached, so `length()` is
#' the rank and `[` selects axes without unwrapping anything first.
#' @param dims (`integer()`)
#' @return `Shape`
#' @export
Shape <- function(dims = integer()) {
  dims <- as.integer(dims)

  if (any(dims[!is.na(dims)] < 0L)) {
    cli_abort("Dimensions must be >= 0")
  }

  structure(dims, class = "Shape")
}

#' @export
`==.Shape` <- function(e1, e2) {
  identical(unclass(e1), unclass(e2))
}

#' @export
# jarl-ignore comparison_negation: != must delegate to == for S3 consistency
`!=.Shape` <- function(e1, e2) {
  !(e1 == e2) # nolint
}

#' @export
repr.Shape <- function(x, ...) {
  dims <- unclass(x)
  if (length(dims) == 0L) {
    return("")
  }
  dims[is.na(dims)] <- "?"
  paste0(dims, collapse = "x")
}

#' @export
format.Shape <- function(x, ...) {
  paste0("(", repr(x), ")")
}

#' @export
print.Shape <- function(x, ...) {
  cat(format(x), "\n", sep = "")
  invisible(x)
}

#' @export
#' @method shape Shape
shape.Shape <- function(x, ...) {
  unclass(x)
}

#' @export
#' @method dtype Constant
dtype.Constant <- function(x, ...) {
  x$type$dtype
}

#' @export
#' @method dtype FuncValue
dtype.FuncValue <- function(x, ...) {
  dtype(x$value_type)
}
