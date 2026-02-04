#' @importFrom cli format_error cli_format
NULL

#' @exportS3Method cli::cli_format
cli_format.TensorDataType <- function(x, style = list(), ...) {
  repr(x)
}

#' @exportS3Method cli::cli_format
cli_format.Shape <- function(x, style = list(), ...) {
  format(x)
}

#' @exportS3Method cli::cli_format
cli_format.ValueType <- function(x, style = list(), ...) {
  repr(x)
}

#' @title index_vec
#' @description Wraps an integer vector marking it as containing 0-based index values.
#' @param x (`integer()`)\cr Integer vector of indices.
#' @return An integer vector with additional class `"IndexVector"`.
#' @export
index_vec <- function(x) {
  structure(as.integer(x), class = "IndexVector")
}

#' @export
format.IndexVector <- function(x, ...) {
  vals <- unclass(x)
  if (length(vals) == 1L) {
    as.character(vals)
  } else {
    paste0("c(", paste0(vals, collapse = ", "), ")")
  }
}

#' @exportS3Method cli::cli_format
cli_format.IndexVector <- function(x, style = NULL, ...) {
  format(x)
}

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
#' @description Converts all index_vec fields in a condition object from
#'   0-based to 1-based.
#' @param x Condition object with index_vec fields.
#' @param ... Additional arguments (not used).
#' @return Condition object with index_vec fields incremented by 1.
#' @export
to_one_based <- function(x, ...) {
  UseMethod("to_one_based")
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
    dimensions = index_vec(dimensions),
    call = call,
    class = c(class, "ErrorDimensionUniqueness"),
    signal = signal
  )
}

#' @export
conditionMessage.ErrorDimensionUniqueness <- function(c, ...) {
  format_error(
    c(
      "{.arg {c$arg}} must contain unique dimension indices",
      x = "Got {.val {c$dimensions}}"
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
    index = index_vec(index),
    lower = index_vec(lower),
    upper = index_vec(upper),
    call = call,
    class = c(class, "ErrorIndexOutOfBounds"),
    signal = signal
  )
}

#' @export
conditionMessage.ErrorIndexOutOfBounds <- function(c, ...) {
  format_error(
    c(
      "{.arg {c$arg}} contains index{?es} outside the valid range.",
      x = "Got {.val {c$index}}, but valid range is [{.val {c$lower}}, {.val {c$upper}})."
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
    permutation = index_vec(permutation),
    expected = index_vec(expected),
    call = call,
    class = c(class, "ErrorPermuteIndex"),
    signal = signal
  )
}

#' @export
conditionMessage.ErrorPermuteIndex <- function(c, ...) {
  format_error(
    c(
      "{.arg {c$arg}} must be a permutation of {.val {c$expected}}.",
      x = "Got {.val {c$permutation}}."
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

#' @title ErrorUnexpectedListType
#' @description Error when an element in a list has an unexpected type
#' @param arg (`character(1)`)\cr Name of the argument
#' @param index (`integer(1)`)\cr The index where the type is unexpected (0-based)
#' @param expected (`character(1)`)\cr Description of what was expected
#' @param actual (`character(1)`)\cr What was observed
#' @param call (`call` or `NULL`)\cr Call that generated the error
#' @param class (`character()`)\cr Additional classes to prepend
#' @param signal (`logical(1)`)\cr Whether to signal the error (default TRUE)
#' @export
error_unexpected_list_type <- function(
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
    index = index_vec(index),
    expected = expected,
    actual = actual,
    call = call,
    class = c(class, "ErrorUnexpectedListType"),
    signal = signal
  )
}

#' @export
conditionMessage.ErrorUnexpectedListType <- function(c, ...) {
  x_line <- if (is.character(c$actual)) {
    "Got {c$actual}."
  } else {
    "Got {.val {c$actual}}."
  }
  format_error(
    c(
      "{.arg {c$arg}[{.val {c$index}}]} {c$expected}.",
      x = x_line
    ),
    .envir = environment()
  )
}

#' @export
to_one_based.ErrorUnexpectedListType <- function(x, ...) {
  x$index <- x$index + 1L
  x
}

#' @title ErrorUnequalTypes
#' @description Error when types at the same index in two lists don't match
#' @param arg1 (`character(1)`)\cr Name of the first argument
#' @param arg2 (`character(1)`)\cr Name of the second argument
#' @param index (`integer(1)`)\cr The index where types don't match (0-based)
#' @param expected (`character(1)`)\cr Description of what was expected
#' @param actual1 Type from the first argument (any object with a cli_format method)
#' @param actual2 Type from the second argument (any object with a cli_format method)
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
    index = index_vec(index),
    expected = expected,
    actual1 = actual1,
    actual2 = actual2,
    call = call,
    class = c(class, "ErrorUnequalTypes"),
    signal = signal
  )
}

#' @export
conditionMessage.ErrorUnequalTypes <- function(c, ...) {
  format_error(
    c(
      "{.arg {c$arg1}[{.val {c$index}}]} and {.arg {c$arg2}[{.val {c$index}}]} {c$expected}.",
      x = "Got {.val {c$actual1}} and {.val {c$actual2}}."
    ),
    .envir = environment()
  )
}

#' @export
to_one_based.ErrorUnequalTypes <- function(x, ...) {
  x$index <- x$index + 1L
  x
}

#' @title ErrorIndicesNotSorted
#' @description Error when indices are not sorted in ascending order
#' @param arg (`character(1)`)\cr Name of the argument that caused the error
#' @param indices (`integer()`)\cr The indices that are not sorted.
#' @param call (`call` or `NULL`)\cr Call that generated the error
#' @param class (`character()`)\cr Additional classes to prepend
#' @param signal (`logical(1)`)\cr Whether to signal the error (default TRUE)
#' @export
error_indices_not_sorted <- function(
  arg,
  indices,
  call = sys.call(-1)[1L],
  class = character(),
  signal = TRUE
) {
  error_stablehlo(
    arg = arg,
    indices = index_vec(indices),
    call = call,
    class = c(class, "ErrorIndicesNotSorted"),
    signal = signal
  )
}

#' @export
conditionMessage.ErrorIndicesNotSorted <- function(c, ...) {
  format_error(
    c(
      "{.arg {c$arg}} must be sorted in ascending order.",
      x = "Got {.val {c$indices}}."
    ),
    .envir = environment()
  )
}

#' @export
to_one_based.ErrorIndicesNotSorted <- function(x, ...) {
  x$indices <- x$indices + 1L
  x
}

#' @title ErrorIndexInSet
#' @description Error when an index is found in a forbidden set
#' @param arg1 (`character(1)`)\cr Name of the argument containing the index
#' @param arg2 (`character(1)`)\cr Name of the argument containing the set
#' @param index (`integer(1)`)\cr The index that was found in the set
#' @param set (`integer()`)\cr The set that should not contain the index
#' @param call (`call` or `NULL`)\cr Call that generated the error
#' @param class (`character()`)\cr Additional classes to prepend
#' @param signal (`logical(1)`)\cr Whether to signal the error (default TRUE)
#' @export
error_index_in_set <- function(
  arg1,
  arg2,
  index,
  set,
  call = sys.call(-1)[1L],
  class = character(),
  signal = TRUE
) {
  error_stablehlo(
    arg1 = arg1,
    arg2 = arg2,
    index = index_vec(index),
    set = index_vec(set),
    call = call,
    class = c(class, "ErrorIndexInSet"),
    signal = signal
  )
}

#' @export
conditionMessage.ErrorIndexInSet <- function(c, ...) {
  format_error(
    c(
      "{.arg {c$arg1}} must not be in {.arg {c$arg2}}.",
      x = "{c$arg1} = {.val {c$index}} is in {c$arg2} = {.val {c$set}}."
    ),
    .envir = environment()
  )
}


#' @export
to_one_based.ErrorIndexInSet <- function(x, ...) {
  x$index <- x$index + 1L
  x$set <- x$set + 1L
  x
}

#' @title ErrorDimSizeMismatch
#' @description Error when a dimension size doesn't match the expected size at a given index
#' @param arg1 (`character(1)`)\cr Name of the first argument (e.g. "operand")
#' @param arg2 (`character(1)`)\cr Name of the second argument (e.g. "result")
#' @param dim1 (`integer(1)`)\cr Dimension index in arg1 (0-based)
#' @param dim2 (`integer(1)`)\cr Dimension index in arg2 (0-based)
#' @param shape1 (`integer()`)\cr Complete shape of arg1
#' @param shape2 (`integer()`)\cr Complete shape of arg2
#' @param call (`call` or `NULL`)\cr Call that generated the error
#' @param class (`character()`)\cr Additional classes to prepend
#' @param signal (`logical(1)`)\cr Whether to signal the error (default TRUE)
#' @export
error_dim_size_mismatch <- function(
  arg1,
  arg2,
  dim1,
  dim2,
  shape1,
  shape2,
  call = sys.call(-1)[1L],
  class = character(),
  signal = TRUE
) {
  error_stablehlo(
    arg1 = arg1,
    arg2 = arg2,
    dim1 = as.integer(dim1),
    dim2 = as.integer(dim2),
    shape1 = as.integer(shape1),
    shape2 = as.integer(shape2),
    call = call,
    class = c(class, "ErrorDimSizeMismatch"),
    signal = signal
  )
}

#' @export
conditionMessage.ErrorDimSizeMismatch <- function(c, ...) {
  fmt_shape <- function(s) paste0("(", paste0(s, collapse = "x"), ")")
  shape1_str <- fmt_shape(c$shape1) # nolint
  shape2_str <- fmt_shape(c$shape2) # nolint
  format_error(
    c(
      "{.arg {c$arg1}} dimension {c$dim1} and {.arg {c$arg2}} dimension {c$dim2} must match unless {.arg {c$arg1}} dim is 1.",
      x = "Got shapes {shapevec_repr(c$shape1)} and {shapevec_repr(c$shape2)}."
    ),
    .envir = environment()
  )
}

#' @export
to_one_based.ErrorDimSizeMismatch <- function(x, ...) {
  x$dim1 <- x$dim1 + 1L
  x$dim2 <- x$dim2 + 1L
  x
}
