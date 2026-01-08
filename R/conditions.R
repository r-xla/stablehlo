#' @importFrom cli format_error
NULL

# Base error constructor
new_stablehlo_error <- function(
  message,
  ...,
  class = character(),
  call = NULL
) {
  structure(
    list(
      message = message,
      call = call,
      ...
    ),
    class = c(class, "stablehlo_error", "error", "condition")
  )
}

#' Shape Mismatch Error
#'
#' Creates and optionally signals an error when dimension sizes don't match between two tensors.
#'
#' @param arg_lhs Name of the left-hand side argument.
#' @param arg_rhs Name of the right-hand side argument.
#' @param dim_lhs Dimension index in the left-hand side tensor.
#' @param dim_rhs Dimension index in the right-hand side tensor.
#' @param size_lhs Size of the dimension in the left-hand side tensor.
#' @param size_rhs Size of the dimension in the right-hand side tensor.
#' @param call The calling context for the error.
#' @param signal If TRUE (default), signals the error. If FALSE, returns the condition object.
#' @export
shape_mismatch_error <- function(
  arg_lhs,
  arg_rhs,
  dim_lhs,
  dim_rhs,
  size_lhs,
  size_rhs,
  call = NULL,
  signal = TRUE
) {
  message <- format_error(c(
    "Dimension {dim_lhs} of {.var {arg_lhs}} must match dimension {dim_rhs} of {.var {arg_rhs}}.",
    i = "Got {.var {arg_lhs}}[{dim_lhs}] = {size_lhs} and {.var {arg_rhs}}[{dim_rhs}] = {size_rhs}."
  ))

  err <- new_stablehlo_error(
    message = message,
    arg_lhs = arg_lhs,
    arg_rhs = arg_rhs,
    dim_lhs = as.integer(dim_lhs),
    dim_rhs = as.integer(dim_rhs),
    size_lhs = as.integer(size_lhs),
    size_rhs = as.integer(size_rhs),
    class = "shape_mismatch_error",
    call = call
  )

  if (signal) {
    rlang::abort(message, call = call, condition = err)
  }

  err
}

#' Unequal Tensor Types Error
#'
#' Creates and optionally signals an error when tensors have different types but are expected to match.
#'
#' @param args Named list of tensor types that should be equal.
#' @param call The calling context for the error.
#' @param signal If TRUE (default), signals the error. If FALSE, returns the condition object.
#' @export
unequal_tensor_types_error <- function(args, call = NULL, signal = TRUE) {
  nms <- names(args)
  types <- paste0(
    # nolint: object_usage_linter
    vapply(seq_along(args), FUN.VALUE = character(1), function(i) {
      paste0(nms[i], "=", repr(args[[i]]))
    }),
    collapse = ", "
  )

  message <- format_error(c(
    "Expected all arguments to have the same tensor type.",
    i = "Got {types}."
  ))

  err <- new_stablehlo_error(
    message = message,
    args = args,
    class = "unequal_tensor_types_error",
    call = call
  )

  if (signal) {
    rlang::abort(message, call = call, condition = err)
  }

  err
}

#' Invalid Identifier Error
#'
#' Creates and optionally signals an error when an identifier doesn't follow naming rules.
#'
#' @param arg The invalid identifier string.
#' @param call The calling context for the error.
#' @param signal If TRUE (default), signals the error. If FALSE, returns the condition object.
#' @export
invalid_identifier_error <- function(arg, call = NULL, signal = TRUE) {
  message <- format_error(c(
    "Identifiers must start with a letter and contain only letters, numbers, and underscores.",
    i = "Got {.val {arg}}."
  ))

  err <- new_stablehlo_error(
    message = message,
    arg = arg,
    class = "invalid_identifier_error",
    call = call
  )

  if (signal) {
    rlang::abort(message, call = call, condition = err)
  }

  err
}

#' Class Error
#'
#' Creates and optionally signals an error when an object has an unexpected class.
#'
#' @param arg Name of the argument being checked.
#' @param expected Character vector of expected class names.
#' @param observed The observed class name.
#' @param call The calling context for the error.
#' @param signal If TRUE (default), signals the error. If FALSE, returns the condition object.
#' @export
class_error <- function(arg, expected, observed, call = NULL, signal = TRUE) {
  message <- format_error(c(
    "Expected {.var {arg}} to have class {.or {expected}}.",
    i = "Got {.cls {observed}}."
  ))

  err <- new_stablehlo_error(
    message = message,
    arg = arg,
    expected = expected,
    observed = observed,
    class = "class_error",
    call = call
  )

  if (signal) {
    rlang::abort(message, call = call, condition = err)
  }

  err
}

#' Tensor Data Type Error
#'
#' Creates and optionally signals an error when a tensor has an unexpected data type.
#'
#' @param arg Name of the argument being checked.
#' @param expected Character vector of expected dtype names.
#' @param observed The observed dtype name.
#' @param call The calling context for the error.
#' @param signal If TRUE (default), signals the error. If FALSE, returns the condition object.
#' @export
tensor_dtype_error <- function(
  arg,
  expected,
  observed,
  call = NULL,
  signal = TRUE
) {
  message <- format_error(c(
    "Expected {.var {arg}} to have dtype {.or {expected}}.",
    i = "Got {.cls {observed}}."
  ))

  err <- new_stablehlo_error(
    message = message,
    arg = arg,
    expected = expected,
    observed = observed,
    class = "tensor_dtype_error",
    call = call
  )

  if (signal) {
    rlang::abort(message, call = call, condition = err)
  }

  err
}

#' Tensor Shape Error
#'
#' Creates and optionally signals an error when a tensor has an unexpected shape.
#'
#' @param arg Name of the argument being checked.
#' @param expected Integer vector of expected shape dimensions.
#' @param observed Integer vector of observed shape dimensions.
#' @param call The calling context for the error.
#' @param signal If TRUE (default), signals the error. If FALSE, returns the condition object.
#' @export
tensor_shape_error <- function(
  arg,
  expected,
  observed,
  call = NULL,
  signal = TRUE
) {
  shapevec_repr <- function(shape) {
    # nolint: object_usage_linter
    sprintf("(%s)", paste0(shape, collapse = ","))
  }

  message <- format_error(c(
    "Expected {.var {arg}} to have shape {shapevec_repr(expected)}.",
    i = "Got {shapevec_repr(observed)}."
  ))

  err <- new_stablehlo_error(
    message = message,
    arg = arg,
    expected = as.integer(expected),
    observed = as.integer(observed),
    class = "tensor_shape_error",
    call = call
  )

  if (signal) {
    rlang::abort(message, call = call, condition = err)
  }

  err
}

#' Tensor Number of Dimensions Error
#'
#' Creates and optionally signals an error when a tensor has an unexpected number of dimensions.
#'
#' @param arg Name of the argument being checked.
#' @param expected Integer vector of length 2 specifying [lower, upper) bounds.
#'   Use NA for lower to mean no lower bound, NA for upper to mean no upper bound.
#'   Use c(n, n+1) to require exactly n dimensions.
#' @param observed The observed number of dimensions.
#' @param call The calling context for the error.
#' @param signal If TRUE (default), signals the error. If FALSE, returns the condition object.
#' @export
tensor_ndims_error <- function(
  arg,
  expected,
  observed,
  call = NULL,
  signal = TRUE
) {
  expected <- as.integer(expected)
  if (length(expected) != 2L) {
    stop("expected must be a length-2 integer vector")
  }

  lower <- expected[1L]
  upper <- expected[2L]

  # Format the range string
  # nolint start: object_usage_linter
  if (is.na(lower) && is.na(upper)) {
    range_str <- "any number of dimensions"
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
  # nolint end

  message <- format_error(c(
    "{.var {arg}} must have {range_str}.",
    i = "Got {observed} dimension{?s}."
  ))

  err <- new_stablehlo_error(
    message = message,
    arg = arg,
    expected = expected,
    observed = as.integer(observed),
    class = "tensor_ndims_error",
    call = call
  )

  if (signal) {
    rlang::abort(message, call = call, condition = err)
  }

  err
}

#' Dimension Out of Range Error
#'
#' Creates and optionally signals an error when a dimension index is outside the valid range [0, ndims).
#'
#' @param arg Name of the argument being checked.
#' @param dimension The dimension index(es) that are out of range (0-based).
#' @param ndims The number of dimensions of the tensor.
#' @param call The calling context for the error.
#' @param signal If TRUE (default), signals the error. If FALSE, returns the condition object.
#' @export
dimension_out_of_range_error <- function(
  arg,
  dimension,
  ndims,
  call = NULL,
  signal = TRUE
) {
  dimension <- as.integer(dimension)
  ndims <- as.integer(ndims)

  dims_str <- if (length(dimension) == 1L) {
    # nolint: object_usage_linter
    paste0("dimension index ", dimension)
  } else {
    paste0("dimension indices: ", paste0(dimension, collapse = ", "))
  }

  message <- format_error(c(
    "{.var {arg}} contains invalid dimension index{?es}.",
    i = "Got {dims_str}, but valid range is [0, {ndims})."
  ))

  err <- new_stablehlo_error(
    message = message,
    arg = arg,
    dimension = dimension,
    ndims = ndims,
    class = "dimension_out_of_range_error",
    call = call
  )

  if (signal) {
    rlang::abort(message, call = call, condition = err)
  }

  err
}

#' Dimension Uniqueness Error
#'
#' Creates and optionally signals an error when dimension indices are not unique.
#'
#' @param arg Name of the argument being checked.
#' @param dimensions The dimension indices that are not unique (0-based).
#' @param call The calling context for the error.
#' @param signal If TRUE (default), signals the error. If FALSE, returns the condition object.
#' @export
dimension_uniqueness_error <- function(
  arg,
  dimensions,
  call = NULL,
  signal = TRUE
) {
  dimensions <- as.integer(dimensions)
  dims_str <- paste0(dimensions, collapse = ", ") # nolint: object_usage_linter

  message <- format_error(c(
    "{.var {arg}} contains duplicate dimension indices.",
    i = "Got [{dims_str}]. Each dimension index must appear only once."
  ))

  err <- new_stablehlo_error(
    message = message,
    arg = arg,
    dimensions = dimensions,
    class = "dimension_uniqueness_error",
    call = call
  )

  if (signal) {
    rlang::abort(message, call = call, condition = err)
  }

  err
}

#' Index Out of Bounds Error
#'
#' Creates and optionally signals an error when an index is outside the valid range [lower, upper).
#'
#' @param arg Name of the argument being checked.
#' @param lower Lower bound of valid range (0-based).
#' @param upper Upper bound of valid range, exclusive (0-based).
#' @param call The calling context for the error.
#' @param signal If TRUE (default), signals the error. If FALSE, returns the condition object.
#' @export
index_out_of_bounds_error <- function(
  arg,
  lower,
  upper,
  call = NULL,
  signal = TRUE
) {
  lower <- as.integer(lower)
  upper <- as.integer(upper)

  message <- format_error(c(
    "{.var {arg}} contains index{?es} outside the valid range.",
    i = "Valid range is [{lower}, {upper})."
  ))

  err <- new_stablehlo_error(
    message = message,
    arg = arg,
    lower = lower,
    upper = upper,
    class = "index_out_of_bounds_error",
    call = call
  )

  if (signal) {
    rlang::abort(message, call = call, condition = err)
  }

  err
}

#' Slice Index Error
#'
#' Creates and optionally signals an error when slice indices (start_indices, limit_indices) are invalid.
#'
#' @param arg Name of the argument being checked.
#' @param indices The invalid indices (0-based).
#' @param index_type Type of index: "start" or "limit".
#' @param call The calling context for the error.
#' @param signal If TRUE (default), signals the error. If FALSE, returns the condition object.
#' @export
slice_index_error <- function(
  arg,
  indices,
  index_type,
  call = NULL,
  signal = TRUE
) {
  indices <- as.integer(indices)

  indices_str <- if (length(indices) == 1L) {
    # nolint: object_usage_linter
    paste0("index ", indices)
  } else {
    paste0("indices: ", paste0(indices, collapse = ", "))
  }

  index_type_label <- if (index_type == "start") "start" else "limit" # nolint: object_usage_linter

  message <- format_error(c(
    "{.var {arg}} contains invalid {index_type_label} {if (length(indices) == 1L) 'index' else 'indices'}.",
    i = "Got {indices_str}."
  ))

  err <- new_stablehlo_error(
    message = message,
    arg = arg,
    indices = indices,
    index_type = index_type,
    class = "slice_index_error",
    call = call
  )

  if (signal) {
    rlang::abort(message, call = call, condition = err)
  }

  err
}

#' Permutation Error
#'
#' Creates and optionally signals an error when permutation values are invalid.
#'
#' @param arg Name of the argument being checked.
#' @param permutation The permutation values that are invalid (0-based).
#' @param ndims The number of dimensions of the tensor.
#' @param call The calling context for the error.
#' @param signal If TRUE (default), signals the error. If FALSE, returns the condition object.
#' @export
permutation_error <- function(
  arg,
  permutation,
  ndims,
  call = NULL,
  signal = TRUE
) {
  permutation <- as.integer(permutation)
  ndims <- as.integer(ndims)

  # nolint start: object_usage_linter
  perm_str <- paste0(permutation, collapse = ", ")
  if (ndims == 0L) {
    expected_str <- "(empty)"
  } else {
    expected_str <- paste0(seq(0, ndims - 1), collapse = ", ")
  }
  # nolint end

  message <- format_error(c(
    "{.var {arg}} must be a permutation of [0, 1, ..., {ndims - 1}].",
    i = "Got [{perm_str}], but expected a permutation of [{expected_str}]."
  ))

  err <- new_stablehlo_error(
    message = message,
    arg = arg,
    permutation = permutation,
    ndims = ndims,
    class = "permutation_error",
    call = call
  )

  if (signal) {
    rlang::abort(message, call = call, condition = err)
  }

  err
}
