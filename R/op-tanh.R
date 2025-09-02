#' @include op.R hlo.R utils.R 
NULL 

OpTanh <- new_Op("OpTanh", "tanh")

hlo_tanh_impl <- hlo_fn(OpTanh, infer_types_generic_uni) 

#' @templateVar mnemonic tanh
#' @template op
#' @export
hlo_tanh <- function(operand) {
  hlo_tanh_impl(values = list(operand = operand))
}
