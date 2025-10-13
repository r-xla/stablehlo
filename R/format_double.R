#' @title Format Double Array with Scientific Notation
#'
#' @description Formats a double array using scientific notation with 16 digits precision,
#' similar to \code{formatC(x, digits = 16, format = "e")}.
#'
#' This is used to embed floating point constants into stableHLO programs.
#'
#' @param x (`double()`)\cr
#'   Vector to format.
#' @param precision (`integer(1)`)\cr
#'   Currently supports 32 and 64 bit precisions.
#'
#' @return `character()`
#'
#' @export
#' @examples
#' format_double(1.23, 32)
#' format_double(1.23, 64)
format_double <- function(x, precision = 64) {
  if (!is.double(x)) {
    cli_abort("x must be double")
  }
  if (!(precision %in% c(32, 64))) {
    cli_abort("precision must be either 32 or 64")
  }
  format_double_cpp(x, precision)
}
