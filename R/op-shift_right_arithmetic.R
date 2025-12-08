#' @include op.R hlo.R
NULL

OpShiftRightArithmetic <- new_Op(
  "OpShiftRightArithmetic",
  "shift_right_arithmetic"
)

#' @rdname hlo_shift_right_arithmetic
#' @export
# fmt: skip
infer_types_shift_right_arithmetic <- function(lhs, rhs) { # nolint
  assert_vt_has_ttype(lhs, IntegerType, UnsignedType, BooleanType)
  assert_vt_has_ttype(rhs, IntegerType, UnsignedType, BooleanType)
  assert_vt_equal(lhs, rhs)
  ValueTypes(list(lhs))
}

# fmt: skip
hlo_shift_right_arithmetic_impl <- hlo_fn( # nolint
  OpShiftRightArithmetic,
  infer_types_shift_right_arithmetic
)

#' @templateVar mnemonic shift_right_arithmetic
#' @template op
#' @export
hlo_shift_right_arithmetic <- function(lhs, rhs) {
  hlo_shift_right_arithmetic_impl(values = list(lhs = lhs, rhs = rhs))
}
