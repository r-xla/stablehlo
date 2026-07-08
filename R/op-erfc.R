#' @include op.R hlo.R type_inference.R
NULL

OpErfc <- new_Op("OpErfc", "erfc", dialect = "chlo")

#' @rdname hlo_erfc
#' @export
infer_types_erfc <- infer_types_float_uni

hlo_erfc_impl <- hlo_fn(OpErfc, infer_types_erfc)

#' @templateVar mnemonic erfc
#' @template op_chlo
#' @export
hlo_erfc <- function(operand, output_types = NULL) {
  hlo_erfc_impl(values = list(operand = operand), output_types = output_types)
}
