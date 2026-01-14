#' @include op.R hlo.R
NULL

OpShiftRightLogical <- new_Op(
  "OpShiftRightLogical",
  "shift_right_logical"
)

hlo_shift_right_logical_impl <- hlo_fn(
  OpShiftRightLogical,
  infer_types_integerish_biv
)

#' @templateVar mnemonic shift_right_logical
#' @template op
#' @export
hlo_shift_right_logical <- function(lhs, rhs) {
  hlo_shift_right_logical_impl(values = list(lhs = lhs, rhs = rhs))
}
