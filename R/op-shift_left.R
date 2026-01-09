#' @include op.R hlo.R
NULL

OpShiftLeft <- new_Op("OpShiftLeft", "shift_left")

#' @rdname hlo_shift_left
#' @export
infer_types_shift_left <- function(lhs, rhs) {
  assert_vt_has_ttype(lhs, "IntegerType", "UnsignedType", "BooleanType")
  assert_vt_has_ttype(rhs, "IntegerType", "UnsignedType", "BooleanType")
  assert_vt_equal(lhs, rhs)
  ValueTypes(list(lhs))
}

hlo_shift_left_impl <- hlo_fn(OpShiftLeft, infer_types_shift_left)

#' @templateVar mnemonic shift_left
#' @template op
#' @export
hlo_shift_left <- function(lhs, rhs) {
  hlo_shift_left_impl(values = list(lhs = lhs, rhs = rhs))
}
