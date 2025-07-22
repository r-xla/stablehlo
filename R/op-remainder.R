#' @include op.R hlo.R type_inference.R
NULL

OpRemainder <- new_Op("OpRemainder", "remainder")

hlo_remainder_impl <- hlo_fn(OpRemainder, infer_types_generic_biv)

#' @templateVar mnemonic remainder
#' @templateVar params %s
#' @templateVar attrs %s
#' @template op
#' @export
hlo_remainder <- function(lhs, rhs) {
  hlo_remainder_impl(values = list(lhs = lhs, rhs = rhs))
}
