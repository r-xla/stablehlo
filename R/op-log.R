#' @include op.R hlo.R type_inference.R
NULL

OpLog <- new_Op("OpLog", "log")

#' @rdname hlo_log
#' @export
infer_types_log <- infer_types_float_uni

hlo_log_impl <- hlo_fn(OpLog, infer_types_log)

#' @templateVar mnemonic log
#' @template op
#' @export
hlo_log <- function(operand) {
  hlo_log_impl(values = list(operand = operand))
}
