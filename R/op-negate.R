#' @include op.R hlo.R utils.R 
NULL 

OpNegate <- new_Op("OpNegate", "negate")

hlo_negate_impl <- hlo_fn(OpNegate, infer_types_generic_uni) 

#' @templateVar mnemonic negate
#' @template op
#' @export
hlo_negate <- function(operand) {
  hlo_negate_impl(values = list(operand = operand))
}
