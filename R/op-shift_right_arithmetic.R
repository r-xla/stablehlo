#' @include op.R hlo.R
NULL

OpShiftRightArithmetic <- new_Op(
  "OpShiftRightArithmetic",
  "shift_right_arithmetic"
)

# fmt: skip
hlo_shift_right_arithmetic_impl <- hlo_fn( # nolint
  OpShiftRightArithmetic,
  infer_types_integerish_biv
)

#' @templateVar mnemonic shift_right_arithmetic
#' @template op
#' @export
hlo_shift_right_arithmetic <- function(lhs, rhs) {
  hlo_shift_right_arithmetic_impl(values = list(lhs = lhs, rhs = rhs))
}
