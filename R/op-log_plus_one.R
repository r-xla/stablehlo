#' @include op.R hlo.R type_inference.R
NULL

OpLogPlusOne <- new_Op("OpLogPlusOne", "log_plus_one")

#' @rdname hlo_log_plus_one
#' @export
infer_types_log_plus_one <- infer_types_float_uni

hlo_log_plus_one_impl <- hlo_fn(OpLogPlusOne, infer_types_log_plus_one)

#' @templateVar mnemonic log_plus_one
#' @template op
#' @export
hlo_log_plus_one <- function(operand) {
  hlo_log_plus_one_impl(values = list(operand = operand))
}
