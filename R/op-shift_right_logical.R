#' @include op.R hlo.R type_inference.R
NULL

OpShiftRightLogical <- new_Op(
  "OpShiftRightLogical",
  "shift_right_logical"
)

#' @rdname hlo_shift_right_logical
#' @export
infer_types_shift_right_logical <- infer_types_integerish_biv

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
