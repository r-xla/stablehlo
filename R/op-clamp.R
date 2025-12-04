#' @include op.R hlo.R
NULL

OpClamp <- new_Op("OpClamp", "clamp")

#' @rdname hlo_clamp
#' @export
# binary ops
infer_types_clamp <- function(Min, operand, Max) {
  assert_vts_are_tensors(Min = Min, operand = operand, Max = Max)
  assert_vt_equal(operand, Max)
  assert_vt_equal(Min, Max)
  ValueTypes(list(operand))
}

hlo_clamp_impl <- hlo_fn(OpClamp, infer_types_clamp)

#' @templateVar mnemonic clamp
#' @template op
#' @export
hlo_clamp <- function(Min, operand, Max) {
  hlo_clamp_impl(values = list(Min = Min, operand = operand, Max = Max))
}
