#' @include op.R hlo.R type_inference.R 
NULL 

OpSign <- new_Op("OpSign", "sign")

hlo_sign_impl <- hlo_fn(OpSign, infer_types_generic_uni) 

#' @templateVar mnemonic sign
#' @template op
#' @export
hlo_sign <- function(operand) {
  hlo_sign_impl(values = list(operand = operand))
}
