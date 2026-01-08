#' @importFrom cli format_error
NULL

# Base error constructor
new_stablehlo_error <- function(
  ...,
  class = character(),
  call = NULL
) {
  err <- structure(
    list(
      call = call,
      ...
    ),
    class = c(class, "stablehlo_error", "error", "condition")
  )

  # Populate the message field by calling the appropriate conditionMessage method
  err$message <- conditionMessage(err)
  err
}

#' Convert 0-based indices to 1-based
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

#' Shape Mismatch Error
#'
#' Creates and optionally signals an error when dimension sizes don't match between two tensors.
#'
#' @param arg_lhs Name of the left-hand side argument.
#' @param arg_rhs Name of the right-hand side argument.
#' @param dim_lhs Dimension index in the left-hand side tensor (0-based).
#' @param dim_rhs Dimension index in the right-hand side tensor (0-based).
#' @param size_lhs Size of the dimension in the left-hand side tensor.
#' @param size_rhs Size of the dimension in the right-hand side tensor.
#' @param call The calling context for the error.
#' @param signal If TRUE (default), signals the error. If FALSE, returns the condition object.
#' @export
error_shape_mismatch <- function(
  arg_lhs,
  arg_rhs,
  dim_lhs,
  dim_rhs,
  size_lhs,
  size_rhs,
  call = NULL,
  signal = TRUE
) {
  err <- new_stablehlo_error(
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
    rlang::cnd_signal(err)
  }

  err
}

#' @export
conditionMessage.shape_mismatch_error <- function(c) {
  format_error(c(
    "Dimension {c$dim_lhs} of {.var {c$arg_lhs}} must match dimension {c$dim_rhs} of {.var {c$arg_rhs}}.",
    i = "Got {.var {c$arg_lhs}}[{c$dim_lhs}] = {c$size_lhs} and {.var {c$arg_rhs}}[{c$dim_rhs}] = {c$size_rhs}."
  ))
}

#' @export
to_one_based.shape_mismatch_error <- function(x, ...) {
  x$dim_lhs <- x$dim_lhs + 1L
  x$dim_rhs <- x$dim_rhs + 1L
  x
}

#' Unequal Tensor Types Error
#'
#' Creates and optionally signals an error when tensors have different types but are expected to match.
#'
#' @param args Named list of tensor types that should be equal.
#' @param call The calling context for the error.
#' @param signal If TRUE (default), signals the error. If FALSE, returns the condition object.
#' @export
error_unequal_tensor_types <- function(args, call = NULL, signal = TRUE) {
  err <- new_stablehlo_error(
    args = args,
    class = "unequal_tensor_types_error",
    call = call
  )

  if (signal) {
    rlang::cnd_signal(err)
  }

  err
}

#' @export
conditionMessage.unequal_tensor_types_error <- function(c) {
  nms <- names(c$args)
  types <- paste0(
    vapply(seq_along(c$args), FUN.VALUE = character(1), function(i) {
      paste0(nms[i], "=", repr(c$args[[i]]))
    }),
    collapse = ", "
  )

  format_error(c(
    "Expected all arguments to have the same tensor type.",
    i = "Got {types}."
  ))
}

#' Invalid Identifier Error
#'
#' Creates and optionally signals an error when an identifier doesn't follow naming rules.
#'
#' @param arg The invalid identifier string.
#' @param call The calling context for the error.
#' @param signal If TRUE (default), signals the error. If FALSE, returns the condition object.
#' @export
error_invalid_identifier <- function(arg, call = NULL, signal = TRUE) {
  err <- new_stablehlo_error(
    arg = arg,
    class = "invalid_identifier_error",
    call = call
  )

  if (signal) {
    rlang::cnd_signal(err)
  }

  err
}

#' @export
conditionMessage.invalid_identifier_error <- function(c) {
  format_error(c(
    "Identifiers must start with a letter and contain only letters, numbers, and underscores.",
    i = "Got {.val {c$arg}}."
  ))
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
error_class <- function(arg, expected, observed, call = NULL, signal = TRUE) {
  err <- new_stablehlo_error(
    arg = arg,
    expected = expected,
    observed = observed,
    class = "class_error",
    call = call
  )

  if (signal) {
    rlang::cnd_signal(err)
  }

  err
}

#' @export
conditionMessage.class_error <- function(c) {
  format_error(c(
    "Expected {.var {c$arg}} to have class {.or {c$expected}}.",
    i = "Got {.cls {c$observed}}."
  ))
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
error_tensor_dtype <- function(
  arg,
  expected,
  observed,
  call = NULL,
  signal = TRUE
) {
  err <- new_stablehlo_error(
    arg = arg,
    expected = expected,
    observed = observed,
    class = "tensor_dtype_error",
    call = call
  )

  if (signal) {
    rlang::cnd_signal(err)
  }

  err
}

#' @export
conditionMessage.tensor_dtype_error <- function(c) {
  format_error(c(
    "Expected {.var {c$arg}} to have dtype {.or {c$expected}}.",
    i = "Got {.cls {c$observed}}."
  ))
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
error_tensor_shape <- function(
  arg,
  expected,
  observed,
  call = NULL,
  signal = TRUE
) {
  err <- new_stablehlo_error(
    arg = arg,
    expected = as.integer(expected),
    observed = as.integer(observed),
    class = "tensor_shape_error",
    call = call
  )

  if (signal) {
    rlang::cnd_signal(err)
  }

  err
}

#' @export
conditionMessage.tensor_shape_error <- function(c) {
  shapevec_repr <- function(shape) {
    sprintf("(%s)", paste0(shape, collapse = ","))
  }

  format_error(c(
    "Expected {.var {c$arg}} to have shape {shapevec_repr(c$expected)}.",
    i = "Got {shapevec_repr(c$observed)}."
  ))
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
error_tensor_ndims <- function(
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

  err <- new_stablehlo_error(
    arg = arg,
    expected = expected,
    observed = as.integer(observed),
    class = "tensor_ndims_error",
    call = call
  )

  if (signal) {
    rlang::cnd_signal(err)
  }

  err
}

#' @export
conditionMessage.tensor_ndims_error <- function(c) {
  lower <- c$expected[1L]
  upper <- c$expected[2L]

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

  format_error(c(
    "{.var {c$arg}} must have {range_str}.",
    i = "Got {c$observed} dimension{?s}."
  ))
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
error_dimension_out_of_range <- function(
  arg,
  dimension,
  ndims,
  call = NULL,
  signal = TRUE
) {
  err <- new_stablehlo_error(
    arg = arg,
    dimension = as.integer(dimension),
    ndims = as.integer(ndims),
    class = "dimension_out_of_range_error",
    call = call
  )

  if (signal) {
    rlang::cnd_signal(err)
  }

  err
}

#' @export
conditionMessage.dimension_out_of_range_error <- function(c) {
  dims_str <- if (length(c$dimension) == 1L) {
    paste0("dimension index ", c$dimension)
  } else {
    paste0("dimension indices: ", paste0(c$dimension, collapse = ", "))
  }

  format_error(c(
    "{.var {c$arg}} contains invalid dimension index{?es}.",
    i = "Got {dims_str}, but valid range is [0, {c$ndims})."
  ))
}

#' @export
to_one_based.dimension_out_of_range_error <- function(x, ...) {
  x$dimension <- x$dimension + 1L
  # ndims is a count, not an index, so it doesn't need conversion
  x
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
error_dimension_uniqueness <- function(
  arg,
  dimensions,
  call = NULL,
  signal = TRUE
) {
  err <- new_stablehlo_error(
    arg = arg,
    dimensions = as.integer(dimensions),
    class = "dimension_uniqueness_error",
    call = call
  )

  if (signal) {
    rlang::cnd_signal(err, call = call)
  }

  err
}

#' @export
conditionMessage.dimension_uniqueness_error <- function(c) {
  dims_str <- paste0(c$dimensions, collapse = ", ")

  format_error(c(
    "{.var {c$arg}} contains duplicate dimension indices.",
    i = "Got [{dims_str}]. Each dimension index must appear only once."
  ))
}

#' @export
to_one_based.dimension_uniqueness_error <- function(x, ...) {
  x$dimensions <- x$dimensions + 1L
  x
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
error_index_out_of_bounds <- function(
  arg,
  lower,
  upper,
  call = NULL,
  signal = TRUE
) {
  err <- new_stablehlo_error(
    arg = arg,
    lower = as.integer(lower),
    upper = as.integer(upper),
    class = "index_out_of_bounds_error",
    call = call
  )

  if (signal) {
    rlang::cnd_signal(err)
  }

  err
}

#' @export
conditionMessage.index_out_of_bounds_error <- function(c) {
  format_error(c(
    "{.var {c$arg}} contains index{?es} outside the valid range.",
    i = "Valid range is [{c$lower}, {c$upper})."
  ))
}

#' @export
to_one_based.index_out_of_bounds_error <- function(x, ...) {
  x$lower <- x$lower + 1L
  x$upper <- x$upper + 1L
  x
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
error_slice_index <- function(
  arg,
  indices,
  index_type,
  call = NULL,
  signal = TRUE
) {
  err <- new_stablehlo_error(
    arg = arg,
    indices = as.integer(indices),
    index_type = index_type,
    class = "slice_index_error",
    call = call
  )

  if (signal) {
    rlang::cnd_signal(err)
  }

  err
}

#' @export
conditionMessage.slice_index_error <- function(c) {
  indices_str <- if (length(c$indices) == 1L) {
    paste0("index ", c$indices)
  } else {
    paste0("indices: ", paste0(c$indices, collapse = ", "))
  }

  index_type_label <- if (c$index_type == "start") "start" else "limit"

  format_error(c(
    "{.var {c$arg}} contains invalid {index_type_label} {if (length(c$indices) == 1L) 'index' else 'indices'}.",
    i = "Got {indices_str}."
  ))
}

#' @export
to_one_based.slice_index_error <- function(x, ...) {
  x$indices <- x$indices + 1L
  x
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
error_permutation <- function(
  arg,
  permutation,
  ndims,
  call = NULL,
  signal = TRUE
) {
  err <- new_stablehlo_error(
    arg = arg,
    permutation = as.integer(permutation),
    ndims = as.integer(ndims),
    class = "permutation_error",
    call = call
  )

  if (signal) {
    rlang::cnd_signal(err)
  }

  err
}

#' @export
conditionMessage.permutation_error <- function(c) {
  perm_str <- paste0(c$permutation, collapse = ", ")
  if (c$ndims == 0L) {
    expected_str <- "(empty)"
  } else {
    expected_str <- paste0(seq(0, c$ndims - 1), collapse = ", ")
  }

  format_error(c(
    "{.var {c$arg}} must be a permutation of [0, 1, ..., {c$ndims - 1}].",
    i = "Got [{perm_str}], but expected a permutation of [{expected_str}]."
  ))
}

#' @export
to_one_based.permutation_error <- function(x, ...) {
  x$permutation <- x$permutation + 1L
  # ndims is a count, not an index, so it doesn't need conversion
  x
}
