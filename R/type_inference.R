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
    return(x@type@elt_type)
  } else if (is(x@type, TokenType)) {
    stop("Invalid input")
  } else {
    stop("Not implemented yet")
  }
}

infer_types_generic_uni <- function(operand) {
  stopifnot(inherits(operand@type, TensorType))
  ValueTypes(list(operand))
}

infer_types_generic_biv <- function(lhs, rhs) {
  stopifnot(inherits(lhs@type, TensorType))
  stopifnot(inherits(rhs@type, TensorType))
  stopifnot(lhs@type == rhs@type)
  ValueTypes(list(lhs))
}

infer_types_boolean_biv <- function(lhs, rhs) {
  stopifnot(inherits(lhs@type, TensorType))
  stopifnot(lhs@type == rhs@type)
  assert_one_of(lhs@type@elt_type@type, IntegerType, BooleanType)
  ValueTypes(list(lhs))
}
