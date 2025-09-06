#' @title Generate stableHLO string representation
#' @description
#' This function generates a string representation of a stableHLO program.
#' @param x The object to generate a string representation of.
#' @param ... Additional arguments passed to the method.
#' @return `character(1)`
#' @importFrom S7 new_generic
#' @export
repr <- new_generic("repr", "x", function(x, ...) {
  if (is.null(FUNC_ENV$vars)) {
    local_vars()
  }
  S7::S7_dispatch()
})

method(repr, NULL) <- function(x) {
  ""
}

method(repr, S7::class_integer) <- function(x) {
  as.character(x)
}

method(repr, S7::class_logical) <- function(x) {
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
