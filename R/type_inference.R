#' @param ... integer
broadcast_shapes <- function(...) {
  if (...length() == 1L) {
    return(...elt(1))
  }

  .broadcast <- function(dim_x, dim_y) {
    if (length(dim_x) > length(dim_y)) {
      dim_y = c(rep(1L, length(dim_x) - length(dim_y)), dim_y)
    } else if (length(dim_x) < length(dim_y)) {
      dim_x = c(rep(1L, length(dim_y) - length(dim_x)), dim_x)
    }
    sapply(seq_along(dim_x), function(i) {
      dx = dim_x[[i]]
      dy = dim_y[[i]]
      if (dx != dy && dx != 1L && dy != 1L) {
        stop("Not broadcastable")
      }
      max(dx, dy)
    })
  }
  Reduce(.broadcast, list(...))
}

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

# These inference functions should return either a ValueType or a list of ValueTypes

infer_types_unop <- function(operand) {
  stopifnot(inherits(operand@type, TensorType))
  ValueTypes(list(ValueType(operand@type)))
}

make_infer_unop <- function(infer_shape, infer_type) {

}

infer_types_binop <- function(lhs, rhs) {
  stopifnot(inherits(lhs@type, TensorType))
  stopifnot(inherits(rhs@type, TensorType))
  # stabhelo does no type-castig
  # jax supports it by adding casting to the stablehlo program
  stopifnot(lhs@type@dtype == rhs@type@dtype)

  shapeout = broadcast_shapes(lhs@type@shape@dims, rhs@type@shape@dims)

  ValueTypes(list(ValueType(TensorType(
    lhs@type@dtype,
    Shape(shapeout)
  ))))
}

# We ignore all restrictinos that apply to quantized tensors for now

# unary ops
infer_types_abs <- function(operand) {
  stopifnot(inherits(operand@type, TensorType))
  # TODO: Not perfectly sure what (C2) means
  ValueTypes(list(ValueType(
    TensorType(
      baseline_element_type(operand),
      operand@type@shape
    )
  )))
}

# binary ops
infer_types_add <- function(lhs, rhs) {
  stopifnot(inherits(lhs@type, TensorType))
  stopifnot(inherits(rhs@type, TensorType))
  #stopifnot(identical(lhs@type@dtype, rhs@type@dtype))
  stopifnot(lhs@type@dtype == rhs@type@dtype)

  ValueTypes(list(ValueType(TensorType(
    lhs@type@dtype,
    Shape(broadcast_shapes(lhs@type@shape@dims, rhs@type@shape@dims))
  ))))
}

infer_types_after_all <- function(...) {

}

infer_types_return <- function(...) {
  ValueTypes()
}
