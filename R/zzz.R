#' @useDynLib stablehlo, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @include repr.R
#' @importFrom methods is
#' @importFrom utils strcapture
#' @import checkmate
NULL
.onLoad <- function(...) {
  S7::methods_register()
}
