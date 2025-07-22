#' @include op.R hlo.R type_inference.R
NULL

OpXor <- new_Op("OpXor", "xor")

hlo_xor_impl <- hlo_fn(OpXor, infer_types_boolean_biv)

#' @templateVar mnemonic xor
#' @templateVar params %s
#' @templateVar attrs %s
#' @template op
#' @export
hlo_xor <- function(lhs, rhs) {
  hlo_xor_impl(values = list(lhs = lhs, rhs = rhs))
}
