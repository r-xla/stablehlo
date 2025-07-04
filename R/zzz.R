#' @importFrom methods is
#' @importFrom utils strcapture
NULL
.onLoad <- function(...) {
  S7::methods_register()
}