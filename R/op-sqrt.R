#' @include op.R hlo.R type_inference.R
NULL

OpSqrt <- new_Op("OpSqrt", "sqrt")

hlo_sqrt_impl <- hlo_fn(OpSqrt, infer_types_generic_uni)

#' @templateVar mnemonic sqrt
#' @templateVar params %s
#' @templateVar attrs %s
#' @template op
#' @export
hlo_sqrt <- function(operand) {
  hlo_sqrt_impl(values = list(operand = operand))
}
