#' @include op.R hlo.R type_inference.R
NULL

OpMinimum <- new_Op("OpMinimum", "minimum")

hlo_minimum_impl <- hlo_fn(OpMinimum, infer_types_generic_biv)

#' @templateVar mnemonic minimum
#' @templateVar params %s
#' @templateVar attrs %s
#' @template op
#' @export
hlo_minimum <- function(lhs, rhs) {
  hlo_minimum_impl(values = list(lhs = lhs, rhs = rhs))
}
