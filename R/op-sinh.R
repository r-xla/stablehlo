#' @include op.R hlo.R type_inference.R
NULL

OpSinh <- new_Op("OpSinh", "sinh", dialect = "chlo")

#' @rdname hlo_sinh
#' @export
infer_types_sinh <- infer_types_float_uni

hlo_sinh_impl <- hlo_fn(OpSinh, infer_types_sinh)

#' @templateVar mnemonic sinh
#' @template op_chlo
#' @export
hlo_sinh <- function(operand) {
  hlo_sinh_impl(values = list(operand = operand))
}
