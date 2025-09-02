#' @include op.R hlo.R
NULL

OpSqrt <- new_Op("OpSqrt", "sqrt")

infer_types_sqrt <- function(operand) {
  stopifnot(inherits(operand@type, TensorType))
  ValueTypes(list(operand))
}
hlo_sqrt_impl <- hlo_fn(OpSqrt, infer_types_sqrt)

#' @templateVar mnemonic sqrt
#' @template op
#' @export
hlo_sqrt <- function(operand) {
  hlo_sqrt_impl(values = list(operand = operand))
}
