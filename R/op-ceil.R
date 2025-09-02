#' @include op.R hlo.R
NULL

OpCeil <- new_Op("OpCeil", "ceil")

infer_types_ceil <- function(operand) {
  stopifnot(inherits(operand@type, TensorType))
  ValueTypes(list(operand))
}
hlo_ceil_impl <- hlo_fn(OpCeil, infer_types_ceil)

#' @templateVar mnemonic ceil
#' @template op
#' @export
hlo_ceil <- function(operand) {
  hlo_ceil_impl(values = list(operand = operand))
}
