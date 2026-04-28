#' @include op.R hlo.R type_inference.R
NULL

OpAtanh <- new_Op("OpAtanh", "atanh", dialect = "chlo")

#' @rdname hlo_atanh
#' @export
infer_types_atanh <- infer_types_float_uni

hlo_atanh_impl <- hlo_fn(OpAtanh, infer_types_atanh)

#' @templateVar mnemonic atanh
#' @template op
#' @export
hlo_atanh <- function(operand) {
  hlo_atanh_impl(values = list(operand = operand))
}
