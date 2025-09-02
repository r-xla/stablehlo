#' @include op.R hlo.R
NULL

OpTan <- new_Op("OpTan", "tan")

infer_types_tan <- function(operand) {
  stopifnot(inherits(operand@type, TensorType))
  ValueTypes(list(operand))
}
hlo_tan_impl <- hlo_fn(OpTan, infer_types_tan)

#' @templateVar mnemonic tan
#' @template op
#' @export
hlo_tan <- function(operand) {
  hlo_tan_impl(values = list(operand = operand))
}
