#' @include op.R hlo.R type_inference.R
NULL

OpNegate <- new_Op("OpNegate", "negate")

hlo_negate_impl <- hlo_fn(OpNegate, infer_types_generic_uni)

#' @templateVar mnemonic negate
#' @templateVar params %s
#' @templateVar attrs %s
#' @template op
#' @export
hlo_negate <- function(operand) {
  hlo_negate_impl(values = list(operand = operand))
}
