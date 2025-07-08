#' @title Generate stableHLO string representation
#' @description
#' This function generates a string representation of a stableHLO program.
#' @param x The object to generate a string representation of.
#' @return `character(1)`
#' @importFrom S7 new_generic
#' @export
repr <- new_generic("repr", "x", function(x) {
  S7::S7_dispatch()
})

method(repr, NULL) <- function(x) {
  ""
}

method(repr, S7::class_integer) <- function(x) {
  as.character(x)
}
