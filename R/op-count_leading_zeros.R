#' @include op.R hlo.R type_inference.R
NULL

OpCountLeadingZeros <- new_Op("OpCountLeadingZeros", "count_leading_zeros")

#' @rdname hlo_count_leading_zeros
#' @export
# fmt: skip
infer_types_count_leading_zeros <- infer_types_integer_uni # nolint

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
