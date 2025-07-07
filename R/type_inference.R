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
  # TODO: QuantizedTensorType
  if (is(x@type, TensorType)) {
    return(x@type@dtype)
  } else if (is(x@type, TokenType)) {
    stop("Invalid input")
  } else {
    stop("Not implemented yet")
  }
}

# We ignore all restrictions that apply to quantized tensors for now

# unary ops
infer_types_abs <- function(operand) {
  stopifnot(inherits(operand@type, TensorType))
  ValueTypes(list(operand))
}

# binary ops
infer_types_add <- function(lhs, rhs) {
  stopifnot(inherits(lhs@type, TensorType))
  stopifnot(inherits(rhs@type, TensorType))
  #stopifnot(identical(lhs@type@dtype, rhs@type@dtype))
  stopifnot(lhs@type == rhs@type)

  ValueTypes(list(lhs))
}

infer_types_return <- function(...) {
  lapply(list(...), function(x) {
    stopifnot(inherits(x, ValueType))
  })
  ValueTypes()
}

infer_types_after_all <- function(...) {
  ValueTypes(list(ValueType(TokenType())))
}

infer_types_and <- function(lhs, rhs) {
  stopifnot(inherits(lhs@type, TensorType))
  stopifnot(lhs@type == rhs@type)
  assert_one_of(lhs@type@dtype@type, IntegerType, BooleanType)

  ValueTypes(list(lhs))
}

infer_types_atan2 <- function(lhs, rhs) {
  stopifnot(inherits(lhs@type, TensorType))
  stopifnot(lhs@type == rhs@type)
  stopifnot(lhs@type@dtype == rhs@type@dtype)
  assert_one_of(lhs@type@dtype@type, FloatType, ComplexType)
  ValueTypes(list(lhs))
}
