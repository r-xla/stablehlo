#' @title Format Double Array with Scientific Notation
#'
#' @description Formats a double array using scientific notation with enough digits to round-trip
#' at the requested precision. For 32 bit, the values are first rounded to the nearest
#' single-precision float, so the parser's own rounding cannot round them a second time.
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

  if (precision == 32) {
    x <- round_to_f32(x)
  }

  res <- character(length(x))
  nans <- is.nan(x)
  infs <- is.infinite(x)
  finite_mask <- !nans & !infs

  # IEEE-754 bit patterns for non-finite values, at the requested width. These
  # must match the tensor's element width: reusing the 32-bit patterns for an
  # f64 constant makes it parse as a tiny denormal (~1e-314), not NaN/Inf.
  if (precision == 32) {
    nan_hex <- "0x7FC00000"
    pos_inf_hex <- "0x7F800000"
    neg_inf_hex <- "0xFF800000"
  } else {
    nan_hex <- "0x7FF8000000000000"
    pos_inf_hex <- "0x7FF0000000000000"
    neg_inf_hex <- "0xFFF0000000000000"
  }

  res[nans] <- nan_hex

  if (any(infs)) {
    pos_inf <- infs & (x > 0)
    neg_inf <- infs & (x < 0)
    res[pos_inf] <- pos_inf_hex
    res[neg_inf] <- neg_inf_hex
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

round_to_f32 <- function(x) {
  rounded <- readBin(
    writeBin(as.vector(x), raw(), size = 4L),
    "double",
    n = length(x),
    size = 4L
  )
  attributes(rounded) <- attributes(x)
  rounded
}
