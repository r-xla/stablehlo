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
