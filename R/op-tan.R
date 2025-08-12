#' @include op.R hlo.R
NULL

Tan <- new_Op("Tan", "tan")

# binary ops
infer_types_tan <- function(operand) {
  stopifnot(inherits(operand@type, TensorType))
  ValueTypes(list(operand))
}

hlo_tan_impl <- hlo_fn(Tan, infer_types_tan)

#' @templateVar mnemonic tan
#' @template op
#' @export
hlo_tan <- function(operand) {
  hlo_tan_impl(values = list(operand = operand))
}
