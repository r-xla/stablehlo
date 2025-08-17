#' @include op.R hlo.R 
NULL 

Exponential_minus_one <- new_Op("Exponential_minus_one", "exponential_minus_one")

infer_types_exponential_minus_one <- function(operand) {
  stopifnot(inherits(operand@type, TensorType))
  ValueTypes(list(operand))
}
hlo_exponential_minus_one_impl <- hlo_fn(Exponential_minus_one, infer_types_exponential_minus_one) 

#' @templateVar mnemonic exponential_minus_one
#' @template op
#' @export
hlo_exponential_minus_one <- function(operand) {
  hlo_exponential_minus_one_impl(values = list(operand = operand))
}
