#' @include op.R hlo.R
NULL

OpOr <- new_Op("OpOr", "or")

infer_types_or <- function(lhs, rhs) {
  stopifnot(inherits(lhs@type, TensorType))
  stopifnot(lhs@type == rhs@type)
  assert_one_of(lhs@type@elt_type@type, IntegerType, BooleanType)
  ValueTypes(list(lhs))
}
hlo_or_impl <- hlo_fn(OpOr, infer_types_or)

#' @templateVar mnemonic or
#' @template op
#' @export
hlo_or <- function(lhs, rhs) {
  hlo_or_impl(values = list(lhs = lhs, rhs = rhs))
}
