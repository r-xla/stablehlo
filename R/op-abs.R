#' @include op.R hlo.R 
NULL 

Abs <- new_Op("Abs", "abs")

infer_types_abs <- function(operand) {
  stopifnot(inherits(operand@type, TensorType))
  ValueTypes(list(operand))
}
hlo_abs_impl <- hlo_fn(Abs, infer_types_abs) 

#' @templateVar mnemonic abs
#' @template op
#' @export
hlo_abs <- function(operand) {
  hlo_abs_impl(values = list(operand = operand))
}
