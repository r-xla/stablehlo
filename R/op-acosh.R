#' @include op.R hlo.R type_inference.R
NULL

OpAcosh <- new_Op("OpAcosh", "acosh", dialect = "chlo")

#' @rdname hlo_acosh
#' @export
infer_types_acosh <- infer_types_float_uni

hlo_acosh_impl <- hlo_fn(OpAcosh, infer_types_acosh)

#' @templateVar mnemonic acosh
#' @template op_chlo
#' @export
hlo_acosh <- function(operand, output_types = NULL) {
  hlo_acosh_impl(values = list(operand = operand), output_types = output_types)
}
