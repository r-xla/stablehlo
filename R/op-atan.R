#' @include op.R hlo.R type_inference.R
NULL

OpAtan <- new_Op("OpAtan", "atan", dialect = "chlo")

#' @rdname hlo_atan
#' @export
infer_types_atan <- infer_types_float_uni

hlo_atan_impl <- hlo_fn(OpAtan, infer_types_atan)

#' @templateVar mnemonic atan
#' @template op
#' @export
hlo_atan <- function(operand) {
  hlo_atan_impl(values = list(operand = operand))
}
