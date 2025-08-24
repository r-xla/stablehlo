#' @include op.R hlo.R 
NULL 

Log <- new_Op("Log", "log")

infer_types_log <- function(operand) {
  stopifnot(inherits(operand@type, TensorType))
  ValueTypes(list(operand))
}
hlo_log_impl <- hlo_fn(Log, infer_types_log) 

#' @templateVar mnemonic log
#' @template op
#' @export
hlo_log <- function(operand) {
  hlo_log_impl(values = list(operand = operand))
}
