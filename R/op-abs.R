#' @include op.R hlo.R
NULL

OpAbs <- new_Op("OpAbs", "abs")

infer_types_abs <- function(operand) {
  stopifnot(inherits(operand@type, TensorType))
  ValueTypes(list(operand))
}
hlo_abs_impl <- hlo_fn(OpAbs, infer_types_abs)

#' @templateVar mnemonic abs
#' @template op
#' @export
hlo_abs <- function(operand) {
  hlo_abs_impl(values = list(operand = operand))
}
