#' @include op.R hlo.R
NULL

OpRsqrt <- new_Op("OpRsqrt", "rsqrt")

infer_types_rsqrt <- function(operand) {
  stopifnot(inherits(operand@type, TensorType))
  ValueTypes(list(operand))
}
hlo_rsqrt_impl <- hlo_fn(OpRsqrt, infer_types_rsqrt)

#' @templateVar mnemonic rsqrt
#' @template op
#' @export
hlo_rsqrt <- function(operand) {
  hlo_rsqrt_impl(values = list(operand = operand))
}
