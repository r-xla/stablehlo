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
  if (is.null(FUNC_ENV$vars)) {
    local_vars()
  }
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

FUNC_ENV <- new.env()

local_vars <- function(local_envir = parent.frame()) {
  FUNC_ENV$vars <- hashtab()
  FUNC_ENV$counter <- 0L
  withr::defer(envir = local_envir, {
    FUNC_ENV$vars <- NULL
    FUNC_ENV$counter <- NULL
  })
  NULL
}
