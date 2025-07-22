#' @include op.R hlo.R type_inference.R
NULL

OpTanh <- new_Op("OpTanh", "tanh")

hlo_tanh_impl <- hlo_fn(OpTanh, infer_types_generic_uni)

#' @templateVar mnemonic tanh
#' @templateVar params %s
#' @templateVar attrs %s
#' @template op
#' @export
hlo_tanh <- function(operand) {
  hlo_tanh_impl(values = list(operand = operand))
}
