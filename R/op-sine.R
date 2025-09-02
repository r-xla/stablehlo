#' @include op.R hlo.R utils.R 
NULL 

OpSine <- new_Op("OpSine", "sine")

hlo_sine_impl <- hlo_fn(OpSine, infer_types_generic_uni) 

#' @templateVar mnemonic sine
#' @template op
#' @export
hlo_sine <- function(operand) {
  hlo_sine_impl(values = list(operand = operand))
}
