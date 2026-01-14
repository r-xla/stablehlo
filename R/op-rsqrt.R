#' @include op.R hlo.R type_inference.R
NULL

OpRsqrt <- new_Op("OpRsqrt", "rsqrt")

#' @rdname hlo_rsqrt
#' @export
infer_types_rsqrt <- infer_types_float_uni

hlo_rsqrt_impl <- hlo_fn(OpRsqrt, infer_types_rsqrt)

#' @templateVar mnemonic rsqrt
#' @template op
#' @export
hlo_rsqrt <- function(operand) {
  hlo_rsqrt_impl(values = list(operand = operand))
}
