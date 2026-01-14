#' @include op.R hlo.R type_inference.R
NULL

OpSqrt <- new_Op("OpSqrt", "sqrt")

#' @rdname hlo_sqrt
#' @export
infer_types_sqrt <- infer_types_float_uni

hlo_sqrt_impl <- hlo_fn(OpSqrt, infer_types_sqrt)

#' @templateVar mnemonic sqrt
#' @template op
#' @export
hlo_sqrt <- function(operand) {
  hlo_sqrt_impl(values = list(operand = operand))
}
