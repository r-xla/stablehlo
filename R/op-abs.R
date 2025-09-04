#' @include op.R hlo.R type_inference.R
NULL

OpAbs <- new_Op("OpAbs", "abs")

hlo_abs_impl <- hlo_fn(OpAbs, infer_types_generic_uni)

#' @templateVar mnemonic abs
#' @template op
#' @export
hlo_abs <- function(operand) {
  hlo_abs_impl(values = list(operand = operand))
}
