#' @include repr.R
#' @importFrom methods is
#' @importFrom utils strcapture
#' @import checkmate
#' @import tengen
#' @importFrom S7 new_class
#' @import S7
#' @importFrom utils hashtab
#' @importFrom cli cli_abort
#' @importFrom xlamisc list_of
NULL

.onLoad <- function(...) {
  S7::methods_register()
}
