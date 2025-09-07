#' @include op.R hlo.R type_inference.R 
NULL 

OpLogPlusOne <- new_Op("OpLogPlusOne", "log_plus_one")

hlo_log_plus_one_impl <- hlo_fn(OpLogPlusOne, infer_types_generic_uni) 

#' @templateVar mnemonic log_plus_one
#' @template op
#' @export
hlo_log_plus_one <- function(operand) {
  hlo_log_plus_one_impl(values = list(operand = operand))
}
