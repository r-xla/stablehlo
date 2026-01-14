#' @include op.R hlo.R type_inference.R
NULL

OpXor <- new_Op("OpXor", "xor")

#' @rdname hlo_xor
#' @export
infer_types_xor <- infer_types_integerish_biv

hlo_xor_impl <- hlo_fn(OpXor, infer_types_xor)

#' @templateVar mnemonic xor
#' @template op
#' @export
hlo_xor <- function(lhs, rhs) {
  hlo_xor_impl(values = list(lhs = lhs, rhs = rhs))
}
