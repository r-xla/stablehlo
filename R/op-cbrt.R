#' @include op.R hlo.R 
NULL 

Cbrt <- new_Op("Cbrt", "cbrt")

infer_types_cbrt <- function(operand) {
  stopifnot(inherits(operand@type, TensorType))
  ValueTypes(list(operand))
}
hlo_cbrt_impl <- hlo_fn(Cbrt, infer_types_cbrt) 

#' @templateVar mnemonic cbrt
#' @template op
#' @export
hlo_cbrt <- function(operand) {
  hlo_cbrt_impl(values = list(operand = operand))
}