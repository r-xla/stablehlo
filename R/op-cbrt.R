#' @include op.R hlo.R type_inference.R
NULL

OpCbrt <- new_Op("OpCbrt", "cbrt")

#' @rdname hlo_cbrt
#' @export
infer_types_cbrt <- infer_types_float_uni

hlo_cbrt_impl <- hlo_fn(OpCbrt, infer_types_cbrt)

#' @templateVar mnemonic cbrt
#' @template op
#' @export
hlo_cbrt <- function(operand) {
  hlo_cbrt_impl(values = list(operand = operand))
}
