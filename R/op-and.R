#' @include op.R hlo.R
NULL

OpAnd <- new_Op("OpAnd", "and")

infer_types_and <- function(lhs, rhs) {
  stopifnot(inherits(lhs@type, TensorType))
  stopifnot(lhs@type == rhs@type)
  assert_one_of(lhs@type@elt_type@type, IntegerType, BooleanType)
  ValueTypes(list(lhs))
}
hlo_and_impl <- hlo_fn(OpAnd, infer_types_and)

#' @templateVar mnemonic and
#' @template op
#' @export
hlo_and <- function(lhs, rhs) {
  hlo_and_impl(values = list(lhs = lhs, rhs = rhs))
}
