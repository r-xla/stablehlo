#' @include op.R hlo.R type_inference.R
NULL

OpAcos <- new_Op("OpAcos", "acos", dialect = "chlo")

#' @rdname hlo_acos
#' @export
infer_types_acos <- infer_types_float_uni

hlo_acos_impl <- hlo_fn(OpAcos, infer_types_acos)

#' @templateVar mnemonic acos
#' @template op_chlo
#' @export
hlo_acos <- function(operand, output_types = NULL) {
  hlo_acos_impl(values = list(operand = operand), output_types = output_types)
}
