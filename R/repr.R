#' @title Generate string representation for object
#' @description
#' This function generates a string representation of an object.
#' In this package, this is primarily used to convert a `Func`
#' to its stableHLO string representation.
#' @param x The object to generate a string representation of.
#' @param ... Additional arguments passed to the method.
#' @return `character(1)`
#' @export
repr <- function(x, ...) {
  UseMethod("repr")
}

#' @export
repr.default <- function(x, ...) {
  stop("repr not implemented for class: ", paste(class(x), collapse = ", "))
}

#' @export
repr.NULL <- function(x, ...) {
  ""
}

#' @export
repr.integer <- function(x, ...) {
  as.character(x)
}

#' @export
repr.logical <- function(x, ...) {
  if (x) "true" else "false"
}
