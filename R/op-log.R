#' @include op.R hlo.R 
NULL 

log <- new_Op("log", "log")

infer_types_log <- function(operand) {
  stopifnot(inherits(operand@type, TensorType))
  ValueTypes(list(operand))
}
hlo_log_impl <- hlo_fn(log, infer_types_log) 

#' @templateVar mnemonic log
#' @template op
#' @export
hlo_log <- function(operand) {
  hlo_log_impl(values = list(operand = operand))
}