#' @include op.R hlo.R type_inference.R
NULL

OpTanh <- new_Op("OpTanh", "tanh")

#' @rdname hlo_tanh
#' @export
infer_types_tanh <- infer_types_float_uni

hlo_tanh_impl <- hlo_fn(OpTanh, infer_types_tanh)

#' @templateVar mnemonic tanh
#' @template op
#' @export
hlo_tanh <- function(operand) {
  hlo_tanh_impl(values = list(operand = operand))
}
