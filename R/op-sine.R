#' @include op.R hlo.R type_inference.R
NULL

OpSine <- new_Op("OpSine", "sine")

#' @rdname hlo_sine
#' @export
infer_types_sine <- infer_types_float_uni

hlo_sine_impl <- hlo_fn(OpSine, infer_types_sine)

#' @templateVar mnemonic sine
#' @template op
#' @export
hlo_sine <- function(operand, output_types = NULL) {
  hlo_sine_impl(values = list(operand = operand), output_types = output_types)
}
