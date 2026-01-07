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
#' Throws an error when dimension sizes don't match between two tensors.
#'
#' @param arg_lhs Name of the left-hand side argument.
#' @param arg_rhs Name of the right-hand side argument.
#' @param dim_lhs Dimension index in the left-hand side tensor.
#' @param dim_rhs Dimension index in the right-hand side tensor.
#' @param size_lhs Size of the dimension in the left-hand side tensor.
#' @param size_rhs Size of the dimension in the right-hand side tensor.
#' @param call The calling context for the error.
#' @export
shape_mismatch_error <- function(
  arg_lhs,
  arg_rhs,
  dim_lhs,
  dim_rhs,
  size_lhs,
  size_rhs,
  call = NULL
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

  rlang::abort(message, call = call, condition = err)
}

#' Unequal Tensor Types Error
#'
#' Throws an error when tensors have different types but are expected to match.
#'
#' @param args Named list of tensor types that should be equal.
#' @param call The calling context for the error.
#' @export
unequal_tensor_types_error <- function(args, call = NULL) {
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

  rlang::abort(message, call = call, condition = err)
}

#' Invalid Identifier Error
#'
#' Throws an error when an identifier doesn't follow naming rules.
#'
#' @param arg The invalid identifier string.
#' @param call The calling context for the error.
#' @export
invalid_identifier_error <- function(arg, call = NULL) {
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

  rlang::abort(message, call = call, condition = err)
}

#' Class Error
#'
#' Throws an error when an object has an unexpected class.
#'
#' @param arg Name of the argument being checked.
#' @param expected Character vector of expected class names.
#' @param observed The observed class name.
#' @param call The calling context for the error.
#' @export
class_error <- function(arg, expected, observed, call = NULL) {
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

  rlang::abort(message, call = call, condition = err)
}

#' Tensor Data Type Error
#'
#' Throws an error when a tensor has an unexpected data type.
#'
#' @param arg Name of the argument being checked.
#' @param expected Character vector of expected dtype names.
#' @param observed The observed dtype name.
#' @param call The calling context for the error.
#' @export
tensor_dtype_error <- function(arg, expected, observed, call = NULL) {
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

  rlang::abort(message, call = call, condition = err)
}

#' Tensor Shape Error
#'
#' Throws an error when a tensor has an unexpected shape.
#'
#' @param arg Name of the argument being checked.
#' @param expected Integer vector of expected shape dimensions.
#' @param observed Integer vector of observed shape dimensions.
#' @param call The calling context for the error.
#' @export
tensor_shape_error <- function(arg, expected, observed, call = NULL) {
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

  rlang::abort(message, call = call, condition = err)
}
