#' @include op.R hlo.R type_inference.R 
NULL 

OpLog <- new_Op("OpLog", "log")

hlo_log_impl <- hlo_fn(OpLog, infer_types_generic_uni) 

#' @templateVar mnemonic log
#' @template op
#' @export
hlo_log <- function(operand) {
  hlo_log_impl(values = list(operand = operand))
}
