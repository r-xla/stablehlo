#' @importFrom cli format_error
NULL

throw_error <- function(c, call = NULL) {
  c$call <- call %??% c$call
  if (is.null(c$call)) {
    c$call <- sys.call(-1)
  }
  rlang::cnd_signal(c)
}

#' @title StablehloError
#' @description Base error class for all stablehlo errors
#' @param message (`character(1)`)\cr Error message
#' @param call (`call` or `NULL`)\cr Call that generated the error
#' @param ... Additional fields to store in the condition
#' @param signal (`logical(1)`)\cr Whether to signal the error (default TRUE)
#' @export
stablehlo_error <- function(
  message = character(),
  call = sys.call(-1),
  ...,
  signal = TRUE
) {
  cond <- structure(
    list(
      message = message,
      call = call,
      ...
    ),
    class = c("StablehloError", "error", "condition")
  )
  if (signal) {
    throw_error(cond, call = call)
  }
  cond
}

#' @export
conditionMessage.StablehloError <- function(c, ...) {
  c$message
}
#' @title DimensionOutOfRangeError
#' @description Error when a dimension index is outside the valid range [0, ndims)
#' @param arg (`character(1)`)\cr Name of the argument that caused the error
#' @param dimension (`integer()`)\cr The dimension index(es) that are out of range (0-based)
#' @param ndims (`integer(1)`)\cr The number of dimensions of the tensor
#' @param call (`call` or `NULL`)\cr Call that generated the error
#' @param signal (`logical(1)`)\cr Whether to signal the error (default TRUE)
#' @export
dimension_out_of_range_error <- function(
  arg,
  dimension,
  ndims,
  call = sys.call(-1),
  signal = TRUE
) {
  cond <- structure(
    list(
      arg = arg,
      dimension = as.integer(dimension),
      ndims = as.integer(ndims),
      message = character(),
      call = call
    ),
    class = c(
      "DimensionOutOfRangeError",
      "StablehloError",
      "error",
      "condition"
    )
  )
  if (signal) {
    throw_error(cond, call = call)
  }
  cond
}

#' @export
conditionMessage.DimensionOutOfRangeError <- function(c, ...) {
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
    )
  )
}

#' @title DimensionUniquenessError
#' @description Error when dimension indices are not unique
#' @param arg (`character(1)`)\cr Name of the argument that caused the error
#' @param dimensions (`integer()`)\cr The dimension indices that are not unique (0-based)
#' @param call (`call` or `NULL`)\cr Call that generated the error
#' @param signal (`logical(1)`)\cr Whether to signal the error (default TRUE)
#' @export
dimension_uniqueness_error <- function(
  arg,
  dimensions,
  call = sys.call(-1),
  signal = TRUE
) {
  cond <- structure(
    list(
      arg = arg,
      dimensions = as.integer(dimensions),
      message = character(),
      call = call
    ),
    class = c(
      "DimensionUniquenessError",
      "StablehloError",
      "error",
      "condition"
    )
  )
  if (signal) {
    throw_error(cond, call = call)
  }
  cond
}

#' @export
conditionMessage.DimensionUniquenessError <- function(c, ...) {
  dims_str <- paste0(c$dimensions, collapse = ", ")
  format_error(
    c(
      "{.var {c$arg}} contains duplicate dimension indices.",
      i = "Got [{dims_str}]. Each dimension index must appear only once."
    ),
    .envir = environment()
  )
}

#' @title IndexOutOfBoundsError
#' @description Error when an index is outside the valid range [lower, upper)
#' @param arg (`character(1)`)\cr Name of the argument that caused the error
#' @param index (`integer()`)\cr The observed index value(s) (0-based)
#' @param lower (`integer(1)`)\cr Lower bound of valid range (0-based)
#' @param upper (`integer(1)`)\cr Upper bound of valid range, exclusive (0-based)
#' @param call (`call` or `NULL`)\cr Call that generated the error
#' @param signal (`logical(1)`)\cr Whether to signal the error (default TRUE)
#' @export
index_out_of_bounds_error <- function(
  arg,
  index,
  lower,
  upper,
  call = sys.call(-1),
  signal = TRUE
) {
  cond <- structure(
    list(
      arg = arg,
      index = as.integer(index),
      lower = as.integer(lower),
      upper = as.integer(upper),
      message = character(),
      call = call
    ),
    class = c("IndexOutOfBoundsError", "StablehloError", "error", "condition")
  )
  if (signal) {
    throw_error(cond, call = call)
  }
  cond
}

#' @export
conditionMessage.IndexOutOfBoundsError <- function(c, ...) {
  index_str <- paste0(c$index, collapse = ", ")
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
to_one_based.DimensionOutOfRangeError <- function(x, ...) {
  x$dimension <- x$dimension + 1L
  # ndims is a count, not an index, so it doesn't need conversion
  x
}

#' @export
to_one_based.DimensionUniquenessError <- function(x, ...) {
  x$dimensions <- x$dimensions + 1L
  x
}

#' @export
to_one_based.IndexOutOfBoundsError <- function(x, ...) {
  x$index <- x$index + 1L
  x$lower <- x$lower + 1L
  x$upper <- x$upper + 1L
  x
}

#' @export
to_one_based.ShapeMismatchError <- function(x, ...) {
  x$dim_lhs <- x$dim_lhs + 1L
  x$dim_rhs <- x$dim_rhs + 1L
  x
}

#' @title SliceIndexError
#' @description Error when slice indices are invalid
#' @param arg (`character(1)`)\cr Name of the argument that caused the error
#' @param index (`integer()`)\cr The invalid index value(s) (0-based)
#' @param lower (`integer(1)`)\cr Lower bound of valid range (0-based)
#' @param upper (`integer(1)`)\cr Upper bound of valid range, exclusive (0-based)
#' @param call (`call` or `NULL`)\cr Call that generated the error
#' @param signal (`logical(1)`)\cr Whether to signal the error (default TRUE)
#' @export
slice_index_error <- function(
  arg,
  index,
  lower,
  upper,
  call = sys.call(-1),
  signal = TRUE
) {
  cond <- structure(
    list(
      arg = arg,
      index = as.integer(index),
      lower = as.integer(lower),
      upper = as.integer(upper),
      message = character(),
      call = call
    ),
    class = c("SliceIndexError", "StablehloError", "error", "condition")
  )
  if (signal) {
    throw_error(cond, call = call)
  }
  cond
}

#' @export
conditionMessage.SliceIndexError <- function(c, ...) {
  index_str <- paste0(c$index, collapse = ", ")
  format_error(
    c(
      "{.var {c$arg}} contains invalid index{?es}.",
      i = "Got {index_str}, but valid range is [{c$lower}, {c$upper})."
    ),
    .envir = environment()
  )
}

#' @export
to_one_based.SliceIndexError <- function(x, ...) {
  x$index <- x$index + 1L
  x$lower <- x$lower + 1L
  x$upper <- x$upper + 1L
  x
}

#' @title PermuteIndexError
#' @description Error when permutation values are invalid (not a valid permutation of indices)
#' @param arg (`character(1)`)\cr Name of the argument that caused the error
#' @param permutation (`integer()`)\cr The permutation values that are invalid (0-based)
#' @param expected (`integer()`)\cr The expected indices to be permuted (0-based)
#' @param call (`call` or `NULL`)\cr Call that generated the error
#' @param signal (`logical(1)`)\cr Whether to signal the error (default TRUE)
#' @export
permute_index_error <- function(
  arg,
  permutation,
  expected,
  call = sys.call(-1),
  signal = TRUE
) {
  cond <- structure(
    list(
      arg = arg,
      permutation = as.integer(permutation),
      expected = as.integer(expected),
      message = character(),
      call = call
    ),
    class = c("PermuteIndexError", "StablehloError", "error", "condition")
  )
  if (signal) {
    throw_error(cond, call = call)
  }
  cond
}

#' @export
conditionMessage.PermuteIndexError <- function(c, ...) {
  perm_str <- paste0(c$permutation, collapse = ", ")
  expected_str <- paste0(c$expected, collapse = ", ")
  format_error(
    c(
      "{.var {c$arg}} must be a permutation of c({expected_str}).",
      i = "Got c({perm_str})."
    ),
    .envir = environment()
  )
}

#' @export
to_one_based.PermuteIndexError <- function(x, ...) {
  x$permutation <- x$permutation + 1L
  x$expected <- x$expected + 1L
  x
}
