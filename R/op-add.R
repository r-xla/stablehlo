#' @include op.R hlo.R type_inference.R
NULL

OpAdd <- new_Op("OpAdd", "add")

hlo_add_impl <- hlo_fn(OpAdd, infer_types_generic_biv)

#' @templateVar mnemonic add
#' @template op
#' @export
hlo_add <- function(lhs, rhs) {
  hlo_add_impl(values = list(lhs = lhs, rhs = rhs))
}
