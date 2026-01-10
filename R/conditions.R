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

#' @title ErrorDimensionOutOfRange
#' @description Error when a dimension index is outside the valid range [0, ndims)
#' @param arg (`character(1)`)\cr Name of the argument that caused the error
#' @param dimension (`integer()`)\cr All dimension index(es) (0-based). The error message will
#'   identify which ones are out of range.
#' @param ndims (`integer(1)`)\cr The number of dimensions of the tensor
#' @param call (`call` or `NULL`)\cr Call that generated the error
#' @param class (`character()`)\cr Additional classes to prepend
#' @param signal (`logical(1)`)\cr Whether to signal the error (default TRUE)
#' @export
error_dimension_out_of_range <- function(
  arg,
  dimension,
  ndims,
  call = sys.call(-1)[1L],
  class = character(),
  signal = TRUE
) {
  error_stablehlo(
    arg = arg,
    dimension = as.integer(dimension),
    ndims = as.integer(ndims),
    call = call,
    class = c(class, "ErrorDimensionOutOfRange"),
    signal = signal
  )
}

#' @export
conditionMessage.ErrorDimensionOutOfRange <- function(c, ...) {
  dims_str <- paste0(c$dimension, collapse = ", ") # nolint
  if (length(c$dimension) == 1) {
    dims_str <- paste0("dimension index ", dims_str)
  } else {
    dims_str <- paste0("dimension indices: ", dims_str)
  }

  format_error(
    c(
      "{.var {c$arg}} contains invalid dimension index{?es}.",
      i = "Got {dims_str}, but valid range is [0, {c$ndims})."
    ),
    .envir = environment()
  )
}

#' @title ErrorDimensionUniqueness
#' @description Error when dimension indices are not unique
#' @param arg (`character(1)`)\cr Name of the argument that caused the error
#' @param dimensions (`integer()`)\cr The dimension indices that are not unique (0-based)
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

#' @title ErrorIndexOutOfBounds
#' @description Error when an index is outside the valid range [lower, upper)
#' @param arg (`character(1)`)\cr Name of the argument that caused the error
#' @param index (`integer()`)\cr The observed index value(s) (0-based)
#' @param lower (`integer(1)`)\cr Lower bound of valid range (0-based)
#' @param upper (`integer(1)`)\cr Upper bound of valid range, exclusive (0-based)
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

#' @export
to_one_based.ErrorDimensionOutOfRange <- function(x, ...) {
  x$dimension <- x$dimension + 1L
  # ndims is a count, not an index, so it doesn't need conversion
  x
}

#' @export
to_one_based.ErrorDimensionUniqueness <- function(x, ...) {
  x$dimensions <- x$dimensions + 1L
  x
}

#' @export
to_one_based.ErrorIndexOutOfBounds <- function(x, ...) {
  x$index <- x$index + 1L
  x$lower <- x$lower + 1L
  x$upper <- x$upper + 1L
  x
}

#' @title ErrorSliceIndex
#' @description Error when slice indices are invalid
#' @param arg (`character(1)`)\cr Name of the argument that caused the error
#' @param index (`integer()`)\cr The invalid index value(s) (0-based)
#' @param lower (`integer(1)`)\cr Lower bound of valid range (0-based)
#' @param upper (`integer(1)`)\cr Upper bound of valid range, exclusive (0-based)
#' @param call (`call` or `NULL`)\cr Call that generated the error
#' @param class (`character()`)\cr Additional classes to prepend
#' @param signal (`logical(1)`)\cr Whether to signal the error (default TRUE)
#' @export
error_slice_index <- function(
  arg,
  index,
  lower,
  upper,
  call = sys.call(-1)[1L],
  class = character(),
  signal = TRUE
) {
  error_stablehlo(
    arg = arg,
    index = as.integer(index),
    lower = as.integer(lower),
    upper = as.integer(upper),
    call = call,
    class = c(class, "ErrorSliceIndex"),
    signal = signal
  )
}

#' @export
conditionMessage.ErrorSliceIndex <- function(c, ...) {
  index_str <- paste0(c$index, collapse = ", ") # nolint
  format_error(
    c(
      "{.var {c$arg}} contains invalid index{?es}.",
      i = "Got {index_str}, but valid range is [{c$lower}, {c$upper})."
    ),
    .envir = environment()
  )
}

#' @export
to_one_based.ErrorSliceIndex <- function(x, ...) {
  x$index <- x$index + 1L
  x$lower <- x$lower + 1L
  x$upper <- x$upper + 1L
  x
}

#' @title ErrorPermuteIndex
#' @description Error when permutation values are invalid (not a valid permutation of indices)
#' @param arg (`character(1)`)\cr Name of the argument that caused the error
#' @param permutation (`integer()`)\cr The permutation values that are invalid (0-based)
#' @param expected (`integer()`)\cr The expected indices to be permuted (0-based)
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
