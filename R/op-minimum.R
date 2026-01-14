#' @include op.R hlo.R type_inference.R
NULL

OpMinimum <- new_Op("OpMinimum", "minimum")

#' @rdname hlo_minimum
#' @export
infer_types_minimum <- infer_types_generic_biv

hlo_minimum_impl <- hlo_fn(OpMinimum, infer_types_minimum)

#' @templateVar mnemonic minimum
#' @template op
#' @export
hlo_minimum <- function(lhs, rhs) {
  hlo_minimum_impl(values = list(lhs = lhs, rhs = rhs))
}
