#' @include op.R hlo.R
NULL

Rsqrt <- new_Op("Rsqrt", "rsqrt")

infer_types_rsqrt <- function(operand) {
  stopifnot(inherits(operand@type, TensorType))
  ValueTypes(list(operand))
}
hlo_rsqrt_impl <- hlo_fn(Rsqrt, infer_types_rsqrt)

#' @templateVar mnemonic rsqrt
#' @template op
#' @export
hlo_rsqrt <- function(operand) {
  hlo_rsqrt_impl(values = list(operand = operand))
}
