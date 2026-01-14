#' @include op.R hlo.R type_inference.R
NULL

OpAdd <- new_Op("OpAdd", "add")

#' @rdname hlo_add
#' @export
infer_types_add <- infer_types_generic_biv

hlo_add_impl <- hlo_fn(OpAdd, infer_types_add)

#' @templateVar mnemonic add
#' @template op
#' @export
hlo_add <- function(lhs, rhs) {
  hlo_add_impl(values = list(lhs = lhs, rhs = rhs))
}
