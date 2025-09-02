#' @include op.R hlo.R
NULL

LogPlusOne <- new_Op("LogPlusOne", "log_plus_one")

infer_types_log_plus_one <- function(operand) {
  stopifnot(inherits(operand@type, TensorType))
  ValueTypes(list(operand))
}
hlo_log_plus_one_impl <- hlo_fn(LogPlusOne, infer_types_log_plus_one)

#' @templateVar mnemonic log_plus_one
#' @template op
#' @export
hlo_log_plus_one <- function(operand) {
  hlo_log_plus_one_impl(values = list(operand = operand))
}
