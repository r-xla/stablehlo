#' @include op.R hlo.R
NULL

OpShiftRightArithmetic <- new_Op(
  "OpShiftRightArithmetic",
  "shift_right_arithmetic"
)

# fmt: skip
infer_types_shift_right_arithmetic <- function(lhs, rhs) { # nolint
  stopifnot(inherits(lhs@type, TensorType))
  stopifnot(lhs@type == rhs@type)
  assert_one_of(lhs@type@dtype, IntegerType, UnsignedType, BooleanType)
  stopifnot(lhs@type@dtype == rhs@type@dtype)
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
