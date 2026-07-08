#' @include op.R hlo.R type_inference.R
NULL

OpAsin <- new_Op("OpAsin", "asin", dialect = "chlo")

#' @rdname hlo_asin
#' @export
infer_types_asin <- infer_types_float_uni

hlo_asin_impl <- hlo_fn(OpAsin, infer_types_asin)

#' @templateVar mnemonic asin
#' @template op_chlo
#' @export
hlo_asin <- function(operand, output_types = NULL) {
  hlo_asin_impl(values = list(operand = operand), output_types = output_types)
}
