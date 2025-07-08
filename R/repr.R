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
