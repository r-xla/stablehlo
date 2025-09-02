#' @include op.R hlo.R
NULL

OpNegate <- new_Op("OpNegate", "negate")

infer_types_negate <- function(operand) {
  stopifnot(inherits(operand@type, TensorType))
  ValueTypes(list(operand))
}
hlo_negate_impl <- hlo_fn(OpNegate, infer_types_negate)

#' @templateVar mnemonic negate
#' @template op
#' @export
hlo_negate <- function(operand) {
  hlo_negate_impl(values = list(operand = operand))
}
