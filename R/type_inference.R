baseline_type <- function(x) {
  if (inherits(x, TensorType)) {
    return(x)
  }
  stop("Not implemented")
  # this function is defined in the stablhlo spec and primarily for quantized tensors
}

# shortcut for element_type(baseline_type(x))
baseline_element_type <- function(x) {
  stopifnot(inherits(x, ValueType))
  if (is(x@type, TensorType)) {
    return(x@type@dtype)
  } else if (is(x@type, TokenType)) {
    stop("Invalid input")
  } else {
    stop("Not implemented yet")
  }
}

#' @title Infer types for unary operations
#' @description
#' Infer the types for unary operations.
#' @param operand (`ValueType`)\cr
#'   The operand.
#' @return (`ValueType`)\cr
#'   The inferred type.
#' @export
infer_types_generic_uni <- function(operand) {
  stopifnot(inherits(operand@type, TensorType))
  ValueTypes(list(operand))
}

#' @title Infer types for binary operations
#' @description
#' Infer the types for binary operations.
#' @param lhs (`ValueType`)\cr
#'   The left-hand side operand.
#' @param rhs (`ValueType`)\cr
#'   The right-hand side operand.
#' @return (`ValueType`)\cr
#'   The inferred type.
#' @export
infer_types_generic_biv <- function(lhs, rhs) {
  stopifnot(inherits(lhs@type, TensorType))
  stopifnot(inherits(rhs@type, TensorType))
  stopifnot(lhs@type == rhs@type)
  ValueTypes(list(lhs))
}

#' @title Infer types for boolean binary operations
#' @description
#' Infer the types for boolean binary operations.
#' @param lhs (`ValueType`)\cr
#'   The left-hand side operand.
#' @param rhs (`ValueType`)\cr
#'   The right-hand side operand.
#' @return (`ValueType`)\cr
#'   The inferred type.
#' @export
infer_types_boolean_biv <- function(lhs, rhs) {
  stopifnot(inherits(lhs@type, TensorType))
  stopifnot(lhs@type == rhs@type)
  assert_one_of(lhs@type@dtype, IntegerType, BooleanType)
  ValueTypes(list(lhs))
}
