baseline_type <- function(x) {
  if (inherits(x, "stablehlo_TensorType")) {
    return(x)
  }
  cli_abort("Not implemented")
  # this function is defined in the stablhlo spec and primarily for quantized tensors
}

# shortcut for element_type(baseline_type(x))
baseline_element_type <- function(x) {
  if (!inherits(x, "stablehlo_ValueType")) {
    cli_abort("x must be a ValueType, but got {.class {class(x)[1]}}.")
  }
  if (inherits(x$type, "stablehlo_TensorType")) {
    return(x$type$dtype)
  } else if (inherits(x$type, "stablehlo_TokenType")) {
    cli_abort("Invalid input")
  } else {
    cli_abort("Not implemented yet")
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
  assert_vt_is_tensor(operand)
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
  assert_vts_are_tensors(lhs = lhs, rhs = rhs)
  assert_vt_equal(lhs, rhs)
  ValueTypes(list(lhs))
}

#' @title Infer types for boolean integerish operations
#' @description
#' Infer the types for integerish (bool or int) binary operations.
#' @param lhs (`ValueType`)\cr
#'   The left-hand side operand.
#' @param rhs (`ValueType`)\cr
#'   The right-hand side operand.
#' @return (`ValueType`)\cr
#'   The inferred type.
#' @export
infer_types_integerish_biv <- function(lhs, rhs) {
  assert_vt_has_ttype(
    lhs,
    "stablehlo_BooleanType",
    "stablehlo_IntegerType",
    "stablehlo_UnsignedType"
  )
  assert_vt_has_ttype(
    rhs,
    "stablehlo_BooleanType",
    "stablehlo_IntegerType",
    "stablehlo_UnsignedType"
  )
  assert_vt_equal(lhs, rhs)
  ValueTypes(list(lhs))
}

#' @title Infer types for integerish unary operations
#' @description
#' Infer the types for integerish (bool or int) unary operations.
#' @param operand (`ValueType`)\cr
#'   The operand.
#' @return (`ValueType`)\cr
#'   The inferred type.
#' @export
infer_types_integerish_uni <- function(operand) {
  assert_vt_has_ttype(
    operand,
    "stablehlo_BooleanType",
    "stablehlo_IntegerType",
    "stablehlo_UnsignedType"
  )
  ValueTypes(list(operand))
}
