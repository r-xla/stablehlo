#' @include op.R hlo.R type_inference.R
NULL

OpCountLeadingZeros <- new_Op("OpCountLeadingZeros", "count_leading_zeros")

hlo_count_leading_zeros_impl <- hlo_fn(
  OpCountLeadingZeros,
  infer_types_integer_uni
)

#' @templateVar mnemonic count_leading_zeros
#' @template op
#' @export
hlo_count_leading_zeros <- function(operand) {
  hlo_count_leading_zeros_impl(values = list(operand = operand))
}
