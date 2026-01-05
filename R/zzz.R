#' @include repr.R
#' @importFrom methods is
#' @importFrom utils strcapture
#' @import checkmate
#' @import tengen
#' @importFrom utils hashtab
#' @importFrom cli cli_abort
NULL

.onLoad <- function(...) {
  # No longer need S7 registration
}
