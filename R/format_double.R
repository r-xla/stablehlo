#' Format Double Array with Scientific Notation
#'
#' Formats a double array using scientific notation with 16 digits precision,
#' similar to \code{formatC(x, digits = 16, format = "e")}.
#'
#' @param x A numeric array (double)
#' @param precision Either 32 or 64 (currently not used but kept for future implementation)
#'
#' @return A character array with the same shape as the input, where each element
#'   is formatted in scientific notation
#'
#' @export
#' @examples
#' format_double(1.23, 32)
#' format_double(1.23, 64)
format_double <- function(x, precision = 64) {
  # Input validation
  if (!is.numeric(x)) {
    stop("x must be numeric")
  }

  if (!(precision %in% c(32, 64))) {
    stop("precision must be either 32 or 64")
  }

  # Call the C++ implementation
  result <- format_c_cpp(x, precision)

  return(result)
}
