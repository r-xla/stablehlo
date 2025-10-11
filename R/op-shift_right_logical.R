#' @include op.R hlo.R
NULL

OpShiftRightLogical <- new_Op(
  "OpShiftRightLogical",
  "shift_right_logical"
)

infer_types_shift_right_logical <- function(lhs, rhs) { # nolint
  stopifnot(inherits(lhs@type, TensorType))
  stopifnot(lhs@type == rhs@type)
  stopifnot(inherits(lhs@type@dtype, IntegerType))
  stopifnot(lhs@type@dtype == rhs@type@dtype)
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
