#' @include op.R hlo.R type_inference.R
NULL

OpShiftLeft <- new_Op("OpShiftLeft", "shift_left")

#' @rdname hlo_shift_left
#' @export
infer_types_shift_left <- infer_types_integerish_biv

hlo_shift_left_impl <- hlo_fn(OpShiftLeft, infer_types_shift_left)

#' @templateVar mnemonic shift_left
#' @template op
#' @export
hlo_shift_left <- function(lhs, rhs) {
  hlo_shift_left_impl(values = list(lhs = lhs, rhs = rhs))
}
