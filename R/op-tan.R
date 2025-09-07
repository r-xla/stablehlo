#' @include op.R hlo.R type_inference.R 
NULL 

OpTan <- new_Op("OpTan", "tan")

hlo_tan_impl <- hlo_fn(OpTan, infer_types_generic_uni) 

#' @templateVar mnemonic tan
#' @template op
#' @export
hlo_tan <- function(operand) {
  hlo_tan_impl(values = list(operand = operand))
}
