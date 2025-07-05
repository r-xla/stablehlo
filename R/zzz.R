#' @importFrom methods is
#' @importFrom utils strcapture
#' @importFrom cli cli_abort
NULL
.onLoad <- function(...) {
  S7::methods_register()
}