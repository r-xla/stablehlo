#' @include op.R hlo.R
NULL

OpCbrt <- new_Op("OpCbrt", "cbrt")

infer_types_cbrt <- function(operand) {
  stopifnot(inherits(operand@type, TensorType))
  ValueTypes(list(operand))
}
hlo_cbrt_impl <- hlo_fn(OpCbrt, infer_types_cbrt)

#' @templateVar mnemonic cbrt
#' @template op
#' @export
hlo_cbrt <- function(operand) {
  hlo_cbrt_impl(values = list(operand = operand))
}
