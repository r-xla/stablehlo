#' @include op.R hlo.R
NULL

Exponential <- new_Op("Exponential", "exponential")

infer_types_exponential <- function(operand) {
  stopifnot(inherits(operand@type, TensorType))
  ValueTypes(list(operand))
}

hlo_exponential_impl <- hlo_fn(Exponential, infer_types_exponential)

#' @templateVar mnemonic exponential
#' @template op
#' @export
hlo_exponential <- function(operand) {
  hlo_exponential_impl(values = list(operand = operand))
}
