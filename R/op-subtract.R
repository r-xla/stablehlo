#' @include op.R hlo.R type_inference.R
NULL

OpSubtract <- new_Op("OpSubtract", "subtract")

#' @rdname hlo_subtract
#' @export
infer_types_subtract <- infer_types_numeric_biv

hlo_subtract_impl <- hlo_fn(OpSubtract, infer_types_subtract)

#' @templateVar mnemonic subtract
#' @template op
#' @export
hlo_subtract <- function(lhs, rhs) {
  hlo_subtract_impl(values = list(lhs = lhs, rhs = rhs))
}
