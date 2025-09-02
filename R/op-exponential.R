#' @include op.R hlo.R
NULL

OpExponential <- new_Op("OpExponential", "exponential")

infer_types_exponential <- function(operand) {
  stopifnot(inherits(operand@type, TensorType))
  ValueTypes(list(operand))
}
hlo_exponential_impl <- hlo_fn(OpExponential, infer_types_exponential)

#' @templateVar mnemonic exponential
#' @template op
#' @export
hlo_exponential <- function(operand) {
  hlo_exponential_impl(values = list(operand = operand))
}
