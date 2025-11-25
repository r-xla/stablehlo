#' @include op.R hlo.R
NULL

OpShiftLeft <- new_Op("OpShiftLeft", "shift_left")

#' @rdname hlo_shift_left
#' @export
infer_types_shift_left <- function(lhs, rhs) {
  stopifnot(inherits(lhs@type, TensorType))
  stopifnot(lhs@type == rhs@type)
  assert_one_of(lhs@type@dtype, IntegerType, UnsignedType, BooleanType)
  stopifnot(lhs@type@dtype == rhs@type@dtype)
  ValueTypes(list(lhs))
}

hlo_shift_left_impl <- hlo_fn(OpShiftLeft, infer_types_shift_left)

#' @templateVar mnemonic shift_left
#' @template op
#' @export
hlo_shift_left <- function(lhs, rhs) {
  hlo_shift_left_impl(values = list(lhs = lhs, rhs = rhs))
}
