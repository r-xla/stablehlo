#' @include op.R hlo.R type_inference.R 
NULL 

OpCeil <- new_Op("OpCeil", "ceil")

hlo_ceil_impl <- hlo_fn(OpCeil, infer_types_generic_uni) 

#' @templateVar mnemonic ceil
#' @template op
#' @export
hlo_ceil <- function(operand) {
  hlo_ceil_impl(values = list(operand = operand))
}
