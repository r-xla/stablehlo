#' @include op.R hlo.R
NULL

Negate <- new_Op("Negate", "negate")

infer_types_negate <- function(operand) {
  stopifnot(inherits(operand@type, TensorType))
  ValueTypes(list(operand))
}
hlo_negate_impl <- hlo_fn(Negate, infer_types_negate)

#' @templateVar mnemonic negate
#' @template op
#' @export
hlo_negate <- function(operand) {
  hlo_negate_impl(values = list(operand = operand))
}
