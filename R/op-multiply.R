#' @include op.R hlo.R 
NULL 

Multiply <- new_Op("Multiply", "multiply")

infer_types_multiply <- function(lhs, rhs) {
  stopifnot(inherits(lhs@type, TensorType))
  stopifnot(inherits(rhs@type, TensorType))
  stopifnot(lhs@type == rhs@type)
  ValueTypes(list(lhs))
}
hlo_multiply_impl <- hlo_fn(Multiply, infer_types_multiply) 

#' @templateVar mnemonic multiply
#' @template op
#' @export
hlo_multiply <- function(lhs, rhs) {
  hlo_multiply_impl(values = list(lhs = lhs, rhs = rhs))
}
