#' @include op.R hlo.R utils.R
NULL

OpOr <- new_Op("OpOr", "or")

hlo_or_impl <- hlo_fn(OpOr, infer_types_boolean_biv)

#' @templateVar mnemonic or
#' @template op
#' @export
hlo_or <- function(lhs, rhs) {
  hlo_or_impl(values = list(lhs = lhs, rhs = rhs))
}
