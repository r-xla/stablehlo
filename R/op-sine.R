#' @include op.R hlo.R 
NULL 

Sine <- new_Op("Sine", "sine")

infer_types_sine <- function(operand) {
  stopifnot(inherits(operand@type, TensorType))
  ValueTypes(list(operand))
}
hlo_sine_impl <- hlo_fn(Sine, infer_types_sine) 

#' @templateVar mnemonic sine
#' @template op
#' @export
hlo_sine <- function(operand) {
  hlo_sine_impl(values = list(operand = operand))
}
