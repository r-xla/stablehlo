#' @importFrom cli format_error
NULL

#' @title ErrorStablehlo
#' @description Base error class for all stablehlo errors
#' @param call (`call` or `NULL`)\cr Call that generated the error
#' @param ... Additional fields to store in the condition
#' @param class (`character()`)\cr Additional classes to prepend
#' @param signal (`logical(1)`)\cr Whether to signal the error (default TRUE)
#' @export
error_stablehlo <- function(
  call = sys.call(-1)[1L],
  ...,
  class = character(),
  signal = TRUE
) {
  cond <- structure(
    list(
      call = call,
      # Use header field so rlang calls conditionMessage lazily
      header = function(cnd, ...) conditionMessage(cnd),
      ...
    ),
    class = c(class, "ErrorStablehlo", "error", "condition")
  )
  if (signal) {
    rlang::cnd_signal(cond)
  }
  cond
}

#' @export
conditionMessage.ErrorStablehlo <- function(c, ...) {
  c$message
}

#' @title Convert 0-based indices to 1-based
#' @description Generic function to convert 0-based indices in error conditions to 1-based
#' @param x Condition object with indices
#' @param ... Additional arguments (not used)
#' @return Condition object with indices converted to 1-based
#' @export
to_one_based <- function(x, ...) {
  UseMethod("to_one_based")
}

#' @export
to_one_based.default <- function(x, ...) {
  x
}

#' @title ErrorDimensionUniqueness
#' @description Error when dimension indices are not unique
#' @param arg (`character(1)`)\cr Name of the argument that caused the error
#' @param dimensions (`integer()`)\cr The dimension indices that are not unique.
#' @param call (`call` or `NULL`)\cr Call that generated the error
#' @param class (`character()`)\cr Additional classes to prepend
#' @param signal (`logical(1)`)\cr Whether to signal the error (default TRUE)
#' @export
error_dimension_uniqueness <- function(
  arg,
  dimensions,
  call = sys.call(-1)[1L],
  class = character(),
  signal = TRUE
) {
  error_stablehlo(
    arg = arg,
    dimensions = as.integer(dimensions),
    call = call,
    class = c(class, "ErrorDimensionUniqueness"),
    signal = signal
  )
}

#' @export
conditionMessage.ErrorDimensionUniqueness <- function(c, ...) {
  dims_str <- paste0(c$dimensions, collapse = ", ") # nolint
  format_error(
    c(
      "{.var {c$arg}} contains duplicate dimension indices.",
      i = "Got [{dims_str}]. Each dimension index must appear only once."
    ),
    .envir = environment()
  )
}

#' @export
to_one_based.ErrorDimensionUniqueness <- function(x, ...) {
  x$dimensions <- x$dimensions + 1L
  x
}

#' @title ErrorIndexOutOfBounds
#' @description Error when an index is outside the valid range [lower, upper)
#' @param arg (`character(1)`)\cr Name of the argument that caused the error
#' @param index (`integer()`)\cr The observed index value(s).
#' @param lower (`integer(1)`)\cr Lower bound of valid range.
#' @param upper (`integer(1)`)\cr Upper bound of valid range, exclusive.
#' @param call (`call` or `NULL`)\cr Call that generated the error
#' @param class (`character()`)\cr Additional classes to prepend
#' @param signal (`logical(1)`)\cr Whether to signal the error (default TRUE)
#' @export
error_index_out_of_bounds <- function(
  arg,
  index,
  lower,
  upper,
  call = sys.call(-1),
  class = character(),
  signal = TRUE
) {
  error_stablehlo(
    arg = arg,
    index = as.integer(index),
    lower = as.integer(lower),
    upper = as.integer(upper),
    call = call,
    class = c(class, "ErrorIndexOutOfBounds"),
    signal = signal
  )
}

#' @export
conditionMessage.ErrorIndexOutOfBounds <- function(c, ...) {
  index_str <- paste0(c$index, collapse = ", ") # nolint
  format_error(
    c(
      "{.var {c$arg}} contains index{?es} outside the valid range.",
      i = "Got {index_str}, but valid range is [{c$lower}, {c$upper})."
    ),
    .envir = environment()
  )
}

#' @export
to_one_based.ErrorIndexOutOfBounds <- function(x, ...) {
  x$index <- x$index + 1L
  x$lower <- x$lower + 1L
  x$upper <- x$upper + 1L
  x
}

#' @title ErrorPermuteIndex
#' @description Error when permutation values are invalid (not a valid permutation of indices)
#' @param arg (`character(1)`)\cr Name of the argument that caused the error
#' @param permutation (`integer()`)\cr The permutation values that are invalid.
#' @param expected (`integer()`)\cr The expected indices to be permuted.
#' @param call (`call` or `NULL`)\cr Call that generated the error
#' @param class (`character()`)\cr Additional classes to prepend
#' @param signal (`logical(1)`)\cr Whether to signal the error (default TRUE)
#' @export
error_permute_index <- function(
  arg,
  permutation,
  expected,
  call = sys.call(-1)[1L],
  class = character(),
  signal = TRUE
) {
  error_stablehlo(
    arg = arg,
    permutation = as.integer(permutation),
    expected = as.integer(expected),
    call = call,
    class = c(class, "ErrorPermuteIndex"),
    signal = signal
  )
}

#' @export
conditionMessage.ErrorPermuteIndex <- function(c, ...) {
  perm_str <- paste0(c$permutation, collapse = ", ") # nolint
  expected_str <- paste0(c$expected, collapse = ", ") # nolint
  format_error(
    c(
      "{.var {c$arg}} must be a permutation of c({expected_str}).",
      i = "Got c({perm_str})."
    ),
    .envir = environment()
  )
}

#' @export
to_one_based.ErrorPermuteIndex <- function(x, ...) {
  x$permutation <- x$permutation + 1L
  x$expected <- x$expected + 1L
  x
}

#' @title ErrorUnexpectedType
#' @description Error when an element in a list has an unexpected type
#' @param arg (`character(1)`)\cr Name of the argument
#' @param index (`integer(1)`)\cr The index where the type is unexpected (0-based)
#' @param expected (`character(1)`)\cr Description of what was expected
#' @param actual (`character(1)`)\cr What was observed
#' @param call (`call` or `NULL`)\cr Call that generated the error
#' @param class (`character()`)\cr Additional classes to prepend
#' @param signal (`logical(1)`)\cr Whether to signal the error (default TRUE)
#' @export
error_unexpected_type <- function(
  arg,
  index,
  expected,
  actual,
  call = sys.call(-1)[1L],
  class = character(),
  signal = TRUE
) {
  error_stablehlo(
    arg = arg,
    index = as.integer(index),
    expected = expected,
    actual = as.character(actual),
    call = call,
    class = c(class, "ErrorUnexpectedType"),
    signal = signal
  )
}

#' @export
conditionMessage.ErrorUnexpectedType <- function(c, ...) {
  format_error(
    c(
      "{.var {c$arg}[{c$index}]} {c$expected}.",
      i = "Got {c$actual}."
    ),
    .envir = environment()
  )
}

#' @export
to_one_based.ErrorUnexpectedType <- function(x, ...) {
  x$index <- x$index + 1L
  x
}

#' @title ErrorUnequalTypes
#' @description Error when types at the same index in two lists don't match
#' @param arg1 (`character(1)`)\cr Name of the first argument
#' @param arg2 (`character(1)`)\cr Name of the second argument
#' @param index (`integer(1)`)\cr The index where types don't match (0-based)
#' @param expected (`character(1)`)\cr Description of what was expected
#' @param actual1 (`character(1)`)\cr Type from the first argument
#' @param actual2 (`character(1)`)\cr Type from the second argument
#' @param call (`call` or `NULL`)\cr Call that generated the error
#' @param class (`character()`)\cr Additional classes to prepend
#' @param signal (`logical(1)`)\cr Whether to signal the error (default TRUE)
#' @export
error_unequal_types <- function(
  arg1,
  arg2,
  index,
  expected,
  actual1,
  actual2,
  call = sys.call(-1)[1L],
  class = character(),
  signal = TRUE
) {
  error_stablehlo(
    arg1 = arg1,
    arg2 = arg2,
    index = as.integer(index),
    expected = expected,
    actual1 = as.character(actual1),
    actual2 = as.character(actual2),
    call = call,
    class = c(class, "ErrorUnequalTypes"),
    signal = signal
  )
}

#' @export
conditionMessage.ErrorUnequalTypes <- function(c, ...) {
  format_error(
    c(
      "{.var {c$arg1}[{c$index}]} and {.var {c$arg2}[{c$index}]} {c$expected}.",
      i = "Got {c$actual1} and {c$actual2}."
    ),
    .envir = environment()
  )
}

#' @export
to_one_based.ErrorUnequalTypes <- function(x, ...) {
  x$index <- x$index + 1L
  x
}
