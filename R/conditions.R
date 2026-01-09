#' @importFrom cli format_error
NULL

throw_error <- function(c, call = NULL) {
  c$call <- call %??% c$call
  if (is.null(c$call)) {
    c$call <- sys.call(-1)
  }
  # Signal the condition directly
  stop(c)
}

#' Convert PascalCase to snake_case
#' @param str Character string in PascalCase
#' @return Character string in snake_case
pascal_to_snake <- function(str) {
  # Insert underscore before uppercase letters (except the first one)
  # Then convert to lowercase
  gsub("([a-z])([A-Z])", "\\1_\\2", str) |>
    tolower()
}

#' Factory function to create error helper functions
#' @param error_constructor The error constructor function
#' @return A function that creates and throws the error
#' @keywords internal
make_error_function <- function(error_constructor) {
  function(..., call = sys.call(-1)) {
    throw_error(
      error_constructor(...),
      call = call
    )
  }
}

#' @title StablehloError
#' @description Base error class for all stablehlo errors
#' @param message (`character(1)`)\cr Error message
#' @param call (`call` or `NULL`)\cr Call that generated the error
#' @param ... Additional fields to store in the condition
#' @export
StablehloError <- function(message = character(), call = NULL, ...) {
  structure(
    list(
      message = as.character(message)[1],
      call = call,
      ...
    ),
    class = c("StablehloError", "error", "condition")
  )
}

#' @export
conditionMessage.StablehloError <- function(c, ...) {
  c$message
}

#' @title InferenceError
#' @description Base class for type inference errors
#' @inheritParams StablehloError
#' @export
InferenceError <- function(message = character(), call = NULL, ...) {
  structure(
    list(
      message = as.character(message)[1],
      call = call,
      ...
    ),
    class = c("InferenceError", "StablehloError", "error", "condition")
  )
}

# When a single argument is invalid
ArgumentError <- function(arg, message = character(), call = NULL, ...) {
  structure(
    list(
      arg = arg,
      message = as.character(message)[1],
      call = call,
      ...
    ),
    class = c("ArgumentError", "StablehloError", "error", "condition")
  )
}

InvalidIdentifierError <- function(
  arg,
  message = character(),
  call = NULL,
  ...
) {
  structure(
    list(
      arg = arg,
      message = as.character(message)[1],
      call = call,
      ...
    ),
    class = c(
      "InvalidIdentifierError",
      "ArgumentError",
      "StablehloError",
      "error",
      "condition"
    )
  )
}

invalid_identifier_error <- make_error_function(InvalidIdentifierError)

#' @export
conditionMessage.InvalidIdentifierError <- function(c, ...) {
  format_error(c(
    "Invalid identifier: {.var {c$arg}}.",
    i = "Identifiers must start with a letter and contain only letters, numbers, and underscores."
  ), .envir = environment())
}

# When two tensors are expected to have the same type, but don't
#' @include types.R
UnequalTensorTypesError <- function(
  args,
  message = character(),
  call = NULL,
  ...
) {
  structure(
    list(
      args = args,
      message = as.character(message)[1],
      call = call,
      ...
    ),
    class = c(
      "UnequalTensorTypesError",
      "InferenceError",
      "StablehloError",
      "error",
      "condition"
    )
  )
}

unequal_tensor_types_error <- make_error_function(UnequalTensorTypesError)

#' @export
conditionMessage.UnequalTensorTypesError <- function(c, ...) {
  nms <- names(c$args)
  types <- paste0(
    vapply(seq_along(c$args), FUN.VALUE = character(1), function(i) {
      paste0(nms[i], " = ", repr(c$args[[i]]))
    }),
    collapse = ", "
  )
  format_error(c(
    "All arguments must have the same tensor type.",
    i = "Got: {types}."
  ), .envir = environment())
}

ClassError <- function(
  arg,
  expected,
  observed,
  message = character(),
  call = NULL,
  ...
) {
  structure(
    list(
      arg = arg,
      expected = expected,
      observed = observed,
      message = as.character(message)[1],
      call = call,
      ...
    ),
    class = c(
      "ClassError",
      "ArgumentError",
      "StablehloError",
      "error",
      "condition"
    )
  )
}

class_error <- make_error_function(ClassError)

#' @export
conditionMessage.ClassError <- function(c, ...) {
  format_error(c(
    "Expected {.var {c$arg}} to have class {.or {c$expected}}.",
    i = "Got {.cls {c$observed}}."
  ), .envir = environment())
}


TensorError <- function(arg, message = character(), call = NULL, ...) {
  structure(
    list(
      arg = arg,
      message = as.character(message)[1],
      call = call,
      ...
    ),
    class = c(
      "TensorError",
      "ArgumentError",
      "StablehloError",
      "error",
      "condition"
    )
  )
}

TensorDTypeError <- function(
  arg,
  expected,
  observed,
  message = character(),
  call = NULL,
  ...
) {
  structure(
    list(
      arg = arg,
      expected = expected,
      observed = observed,
      message = as.character(message)[1],
      call = call,
      ...
    ),
    class = c(
      "TensorDTypeError",
      "ClassError",
      "StablehloError",
      "error",
      "condition"
    )
  )
}

tensor_dtype_error <- make_error_function(TensorDTypeError)

#' @export
conditionMessage.TensorDTypeError <- function(c, ...) {
  format_error(c(
    "Expected {.var {c$arg}} to have dtype {.or {c$expected}}.",
    i = "Got {.cls {c$observed}}."
  ), .envir = environment())
}

TensorNDimsError <- function(
  arg,
  expected,
  observed,
  message = character(),
  call = NULL,
  ...
) {
  expected <- as.integer(expected)
  if (length(expected) != 2L) {
    cli::cli_abort("expected must be a length-2 integer vector")
  }
  observed <- as.integer(observed)

  structure(
    list(
      arg = arg,
      expected = expected,
      observed = observed,
      message = as.character(message)[1],
      call = call,
      ...
    ),
    class = c(
      "TensorNDimsError",
      "TensorError",
      "StablehloError",
      "error",
      "condition"
    )
  )
}

tensor_ndims_error <- make_error_function(TensorNDimsError)

#' @export
conditionMessage.TensorNDimsError <- function(c, ...) {
  expected_range <- c$expected
  lower <- expected_range[1L]
  upper <- expected_range[2L]

  # Format the range string
  if (is.na(lower) && is.na(upper)) {
    range_str <- "any number of dimensions" # this should never happen
  } else if (is.na(lower)) {
    range_str <- paste0("less than ", upper, " dimensions")
  } else if (is.na(upper)) {
    range_str <- paste0("at least ", lower, " dimensions")
  } else if (lower == upper - 1L) {
    range_str <- paste0(
      "exactly ",
      lower,
      " dimension",
      if (lower != 1L) "s" else ""
    )
  } else {
    range_str <- paste0(
      "between ",
      lower,
      " and ",
      upper - 1L,
      " dimensions (inclusive)"
    )
  }

  format_error(c(
    "{.var {c$arg}} must have {range_str}.",
    i = "Got {c$observed} dimension{?s}."
  ), .envir = environment())
}

TensorShapeError <- function(
  arg,
  expected,
  observed,
  message = character(),
  call = NULL,
  ...
) {
  structure(
    list(
      arg = arg,
      expected = as.integer(expected),
      observed = as.integer(observed),
      message = as.character(message)[1],
      call = call,
      ...
    ),
    class = c(
      "TensorShapeError",
      "TensorError",
      "StablehloError",
      "error",
      "condition"
    )
  )
}

tensor_shape_error <- make_error_function(TensorShapeError)

shapevec_repr <- function(shape) {
  sprintf("(%s)", paste0(shape, collapse = ","))
}

#' @export
conditionMessage.TensorShapeError <- function(c, ...) {
  format_error(c(
    "{.var {c$arg}} must have shape {shapevec_repr(c$expected)}.",
    i = "Got shape {shapevec_repr(c$observed)}."
  ), .envir = environment())
}

ShapeMismatchError <- function(
  arg_lhs,
  arg_rhs,
  dim_lhs,
  dim_rhs,
  size_lhs,
  size_rhs,
  message = character(),
  call = NULL,
  ...
) {
  structure(
    list(
      arg_lhs = arg_lhs,
      arg_rhs = arg_rhs,
      dim_lhs = as.integer(dim_lhs),
      dim_rhs = as.integer(dim_rhs),
      size_lhs = as.integer(size_lhs),
      size_rhs = as.integer(size_rhs),
      message = as.character(message)[1],
      call = call,
      ...
    ),
    class = c(
      "ShapeMismatchError",
      "InferenceError",
      "StablehloError",
      "error",
      "condition"
    )
  )
}

shape_mismatch_error <- make_error_function(ShapeMismatchError)

#' @export
conditionMessage.ShapeMismatchError <- function(c, ...) {
  format_error(c(
    "Dimension {c$dim_lhs} of {.var {c$arg_lhs}} must match dimension {c$dim_rhs} of {.var {c$arg_rhs}}.",
    i = "{.var {c$arg_lhs}} has size {c$size_lhs} at dimension {c$dim_lhs}, but {.var {c$arg_rhs}} has size {c$size_rhs} at dimension {c$dim_rhs}."
  ), .envir = environment())
}

#' @title DimensionOutOfRangeError
#' @description Error when a dimension index is outside the valid range [0, ndims)
#' @param arg (`character(1)`)\cr Name of the argument that caused the error
#' @param dimension (`integer()`)\cr The dimension index(es) that are out of range (0-based)
#' @param ndims (`integer(1)`)\cr The number of dimensions of the tensor
#' @param message (`character(1)`)\cr Error message
#' @param call (`call` or `NULL`)\cr Call that generated the error
#' @param ... Additional fields
#' @export
DimensionOutOfRangeError <- function(
  arg,
  dimension,
  ndims,
  message = character(),
  call = NULL,
  ...
) {
  structure(
    list(
      arg = arg,
      dimension = as.integer(dimension),
      ndims = as.integer(ndims),
      message = as.character(message)[1],
      call = call,
      ...
    ),
    class = c(
      "DimensionOutOfRangeError",
      "ArgumentError",
      "StablehloError",
      "error",
      "condition"
    )
  )
}

dimension_out_of_range_error <- make_error_function(DimensionOutOfRangeError)

#' @export
conditionMessage.DimensionOutOfRangeError <- function(c, ...) {
  dims_str <- if (length(c$dimension) == 1L) {
    paste0("dimension index ", c$dimension)
  } else {
    paste0("dimension indices: ", paste0(c$dimension, collapse = ", "))
  }
  format_error(c(
    "{.var {c$arg}} contains invalid dimension index{?es}.",
    i = "Got {dims_str}, but valid range is [0, {c$ndims})."
  ), .envir = environment())
}

#' @title DimensionUniquenessError
#' @description Error when dimension indices are not unique
#' @param arg (`character(1)`)\cr Name of the argument that caused the error
#' @param dimensions (`integer()`)\cr The dimension indices that are not unique (0-based)
#' @param message (`character(1)`)\cr Error message
#' @param call (`call` or `NULL`)\cr Call that generated the error
#' @param ... Additional fields
#' @export
DimensionUniquenessError <- function(
  arg,
  dimensions,
  message = character(),
  call = NULL,
  ...
) {
  structure(
    list(
      arg = arg,
      dimensions = as.integer(dimensions),
      message = as.character(message)[1],
      call = call,
      ...
    ),
    class = c(
      "DimensionUniquenessError",
      "ArgumentError",
      "StablehloError",
      "error",
      "condition"
    )
  )
}

dimension_uniqueness_error <- make_error_function(DimensionUniquenessError)

#' @export
conditionMessage.DimensionUniquenessError <- function(c, ...) {
  dims_str <- paste0(c$dimensions, collapse = ", ")
  format_error(c(
    "{.var {c$arg}} contains duplicate dimension indices.",
    i = "Got [{dims_str}]. Each dimension index must appear only once."
  ), .envir = environment())
}

#' @title IndexOutOfBoundsError
#' @description Error when an index is outside the valid range [lower, upper)
#' @param arg (`character(1)`)\cr Name of the argument that caused the error
#' @param lower (`integer(1)`)\cr Lower bound of valid range (0-based)
#' @param upper (`integer(1)`)\cr Upper bound of valid range, exclusive (0-based)
#' @param message (`character(1)`)\cr Error message
#' @param call (`call` or `NULL`)\cr Call that generated the error
#' @param ... Additional fields
#' @export
IndexOutOfBoundsError <- function(
  arg,
  lower,
  upper,
  message = character(),
  call = NULL,
  ...
) {
  structure(
    list(
      arg = arg,
      lower = as.integer(lower),
      upper = as.integer(upper),
      message = as.character(message)[1],
      call = call,
      ...
    ),
    class = c(
      "IndexOutOfBoundsError",
      "ArgumentError",
      "StablehloError",
      "error",
      "condition"
    )
  )
}

index_out_of_bounds_error <- make_error_function(IndexOutOfBoundsError)

#' @export
conditionMessage.IndexOutOfBoundsError <- function(c, ...) {
  format_error(c(
    "{.var {c$arg}} contains index{?es} outside the valid range.",
    i = "Valid range is [{c$lower}, {c$upper})."
  ), .envir = environment())
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
#' @description Error when slice indices (start_indices, limit_indices) are invalid
#' @param arg (`character(1)`)\cr Name of the argument that caused the error
#' @param indices (`integer()`)\cr The invalid indices (0-based)
#' @param index_type (`character(1)`)\cr Type of index: "start" or "limit"
#' @param message (`character(1)`)\cr Error message
#' @param call (`call` or `NULL`)\cr Call that generated the error
#' @param ... Additional fields
#' @export
SliceIndexError <- function(
  arg,
  indices,
  index_type,
  message = character(),
  call = NULL,
  ...
) {
  structure(
    list(
      arg = arg,
      indices = as.integer(indices),
      index_type = index_type,
      message = as.character(message)[1],
      call = call,
      ...
    ),
    class = c(
      "SliceIndexError",
      "ArgumentError",
      "StablehloError",
      "error",
      "condition"
    )
  )
}

slice_index_error <- make_error_function(SliceIndexError)

#' @export
conditionMessage.SliceIndexError <- function(c, ...) {
  indices_str <- if (length(c$indices) == 1L) {
    paste0("index ", c$indices)
  } else {
    paste0("indices: ", paste0(c$indices, collapse = ", "))
  }
  index_type_label <- if (c$index_type == "start") "start" else "limit"
  format_error(c(
    "{.var {c$arg}} contains invalid {index_type_label} {if (length(c$indices) == 1L) 'index' else 'indices'}.",
    i = "Got {indices_str}."
  ), .envir = environment())
}

#' @export
to_one_based.SliceIndexError <- function(x, ...) {
  x$indices <- x$indices + 1L
  x
}

#' @title PermutationError
#' @description Error when permutation values are invalid (not a valid permutation of dimension indices)
#' @param arg (`character(1)`)\cr Name of the argument that caused the error
#' @param permutation (`integer()`)\cr The permutation values that are invalid (0-based)
#' @param ndims (`integer(1)`)\cr The number of dimensions of the tensor
#' @param message (`character(1)`)\cr Error message
#' @param call (`call` or `NULL`)\cr Call that generated the error
#' @param ... Additional fields
#' @export
PermutationError <- function(
  arg,
  permutation,
  ndims,
  message = character(),
  call = NULL,
  ...
) {
  structure(
    list(
      arg = arg,
      permutation = as.integer(permutation),
      ndims = as.integer(ndims),
      message = as.character(message)[1],
      call = call,
      ...
    ),
    class = c(
      "PermutationError",
      "ArgumentError",
      "StablehloError",
      "error",
      "condition"
    )
  )
}

permutation_error <- make_error_function(PermutationError)

#' @export
conditionMessage.PermutationError <- function(c, ...) {
  perm_str <- paste0(c$permutation, collapse = ", ")
  if (c$ndims == 0L) {
    expected_str <- "(empty)"
  } else {
    expected_str <- paste0(seq(0, c$ndims - 1), collapse = ", ")
  }
  format_error(c(
    "{.var {c$arg}} must be a permutation of [0, 1, ..., {c$ndims - 1}].",
    i = "Got [{perm_str}], but expected a permutation of [{expected_str}]."
  ), .envir = environment())
}

#' @export
to_one_based.PermutationError <- function(x, ...) {
  x$permutation <- x$permutation + 1L
  # ndims is a count, not an index, so it doesn't need conversion
  x
}
