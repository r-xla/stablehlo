#' @include op.R hlo.R type_inference.R
NULL

OpMultiply <- new_Op("OpMultiply", "multiply")

#' @rdname hlo_multiply
#' @export
infer_types_multiply <- infer_types_generic_biv

hlo_multiply_impl <- hlo_fn(OpMultiply, infer_types_multiply)

#' @templateVar mnemonic multiply
#' @template op
#' @export
hlo_multiply <- function(lhs, rhs) {
  hlo_multiply_impl(values = list(lhs = lhs, rhs = rhs))
}
