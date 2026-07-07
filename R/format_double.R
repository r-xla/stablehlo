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

  if (length(x) == 0) {
    return(character(0))
  }

  res <- character(length(x))
  nans <- is.nan(x)
  infs <- is.infinite(x)
  finite_mask <- !nans & !infs

  res[nans] <- "0x7FC00000"

  if (any(infs)) {
    pos_inf <- infs & (x > 0)
    neg_inf <- infs & (x < 0)
    res[pos_inf] <- "0x7F800000"
    res[neg_inf] <- "0xFF800000"
  }

  if (any(finite_mask)) {
    digits <- if (precision == 32) 8 else 16
    res[finite_mask] <- formatC(x[finite_mask], digits = digits, format = "e")
  }
  if (!is.null(dim(x))) {
    dim(res) <- dim(x)
  }
  res
}
