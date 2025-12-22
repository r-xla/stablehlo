#' @include op.R hlo.R type_inference.R
NULL

OpCountLeadingZeros <- new_Op("OpCountLeadingZeros", "count_leading_zeros")

#' @rdname hlo_count_leading_zeros
#' @export
# fmt: skip
infer_types_count_leading_zeros <- function(operand) { # nolint
  assert_vt_is_tensor(operand)
  ValueTypes(list(operand))
}

hlo_count_leading_zeros_impl <- hlo_fn(
  OpCountLeadingZeros,
  infer_types_count_leading_zeros
)

#' @templateVar mnemonic count_leading_zeros
#' @template op
#' @export
hlo_count_leading_zeros <- function(operand) {
  hlo_count_leading_zeros_impl(values = list(operand = operand))
}
