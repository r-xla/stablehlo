#' @include op.R hlo.R type_inference.R
NULL

OpRemainder <- new_Op("OpRemainder", "remainder")

#' @rdname hlo_remainder
#' @export
infer_types_remainder <- infer_types_numeric_biv

hlo_remainder_impl <- hlo_fn(OpRemainder, infer_types_remainder)

#' @templateVar mnemonic remainder
#' @template op
#' @export
hlo_remainder <- function(lhs, rhs) {
  hlo_remainder_impl(values = list(lhs = lhs, rhs = rhs))
}
