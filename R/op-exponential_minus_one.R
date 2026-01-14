#' @include op.R hlo.R type_inference.R
NULL

OpExponentialMinusOne <- new_Op(
  "OpExponentialMinusOne",
  "exponential_minus_one"
)

#' @rdname hlo_exponential_minus_one
#' @export
# fmt: skip
infer_types_exponential_minus_one <- infer_types_float_uni # nolint

hlo_exponential_minus_one_impl <- hlo_fn(
  OpExponentialMinusOne,
  infer_types_exponential_minus_one
)

#' @templateVar mnemonic exponential_minus_one
#' @template op
#' @export
hlo_exponential_minus_one <- function(operand) {
  hlo_exponential_minus_one_impl(values = list(operand = operand))
}
