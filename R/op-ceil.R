#' @include op.R hlo.R type_inference.R
NULL

OpCeil <- new_Op("OpCeil", "ceil")

#' @rdname hlo_ceil
#' @export
infer_types_ceil <- infer_types_float_uni

hlo_ceil_impl <- hlo_fn(OpCeil, infer_types_ceil)

#' @templateVar mnemonic ceil
#' @template op
#' @export
hlo_ceil <- function(operand) {
  hlo_ceil_impl(values = list(operand = operand))
}
