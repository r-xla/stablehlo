#' @include op.R hlo.R type_inference.R
NULL

OpSubtract <- new_Op("OpSubtract", "subtract")

hlo_subtract_impl <- hlo_fn(OpSubtract, infer_types_generic_biv)

#' @templateVar mnemonic subtract
#' @template op

#' @export
hlo_subtract <- function(lhs, rhs) {
  hlo_subtract_impl(values = list(lhs = lhs, rhs = rhs))
}
