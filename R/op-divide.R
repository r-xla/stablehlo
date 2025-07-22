#' @include op.R hlo.R type_inference.R
NULL

OpDivide <- new_Op("OpDivide", "divide")

hlo_divide_impl <- hlo_fn(OpDivide, infer_types_generic_biv)

#' @templateVar mnemonic divide
#' @templateVar params %s
#' @templateVar attrs %s
#' @template op
#' @export
hlo_divide <- function(lhs, rhs) {
  hlo_divide_impl(values = list(lhs = lhs, rhs = rhs))
}
