#' Converts a function to StableHLO src code.
#' 
#' @export
jit <- function(f) {
  if (!rlang::is_function(f)) {
    cli::cli_abort("Expected {.arg f} to be {.cls function}. Got {.cls {class(f)}}.")
  }

}

jit_impl <- function(exprs) {

}
