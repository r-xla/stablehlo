#' @include op.R hlo.R
NULL

OpXor <- new_Op("OpXor", "xor")

infer_types_xor <- function(lhs, rhs) {
  stopifnot(inherits(lhs@type, TensorType))
  stopifnot(lhs@type == rhs@type)
  assert_one_of(lhs@type@elt_type@type, IntegerType, BooleanType)
  ValueTypes(list(lhs))
}
hlo_xor_impl <- hlo_fn(OpXor, infer_types_xor)

#' @templateVar mnemonic xor
#' @template op
#' @export
hlo_xor <- function(lhs, rhs) {
  hlo_xor_impl(values = list(lhs = lhs, rhs = rhs))
}
