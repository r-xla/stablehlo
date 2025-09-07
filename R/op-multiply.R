#' @include op.R hlo.R type_inference.R 
NULL 

OpMultiply <- new_Op("OpMultiply", "multiply")

hlo_multiply_impl <- hlo_fn(OpMultiply, infer_types_generic_biv) 

#' @templateVar mnemonic multiply
#' @template op
#' @export
hlo_multiply <- function(lhs, rhs) {
  hlo_multiply_impl(values = list(lhs = lhs, rhs = rhs))
}
