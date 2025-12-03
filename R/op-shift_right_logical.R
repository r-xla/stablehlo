#' @include op.R hlo.R
NULL

OpShiftRightLogical <- new_Op(
  "OpShiftRightLogical",
  "shift_right_logical"
)

#' @rdname hlo_shift_right_logical
#' @export
# fmt: skip
infer_types_shift_right_logical <- function(lhs, rhs) { # nolint
  assert_vt_is_tensor(lhs)
  assert_vt_equal(lhs, rhs)
  assert_vt_has_dtype(lhs, IntegerType, UnsignedType, BooleanType)
  ValueTypes(list(lhs))
}

hlo_shift_right_logical_impl <- hlo_fn(
  OpShiftRightLogical,
  infer_types_shift_right_logical
)

#' @templateVar mnemonic shift_right_logical
#' @template op
#' @export
hlo_shift_right_logical <- function(lhs, rhs) {
  hlo_shift_right_logical_impl(values = list(lhs = lhs, rhs = rhs))
}
