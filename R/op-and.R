#' @include op.R hlo.R type_inference.R
NULL

OpAnd <- new_Op("OpAnd", "and")

hlo_and_impl <- hlo_fn(OpAnd, infer_types_boolean_biv)

#' @templateVar mnemonic and
#' @template op
#' @export
hlo_and <- function(lhs, rhs) {
  hlo_and_impl(values = list(lhs = lhs, rhs = rhs))
}
