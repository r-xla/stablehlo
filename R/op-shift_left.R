#' @include op.R hlo.R
NULL

OpShiftLeft <- new_Op("OpShiftLeft", "shift_left")

hlo_shift_left_impl <- hlo_fn(OpShiftLeft, infer_types_integerish_biv)

#' @templateVar mnemonic shift_left
#' @template op
#' @export
hlo_shift_left <- function(lhs, rhs) {
  hlo_shift_left_impl(values = list(lhs = lhs, rhs = rhs))
}
