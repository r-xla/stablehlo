#' @include op.R hlo.R
NULL

Ceil <- new_Op("Ceil", "ceil")

# binary ops
infer_types_ceil <- function(operand) {
  stopifnot(inherits(operand@type, TensorType))
  ValueTypes(list(operand))
}

hlo_ceil_impl <- hlo_fn(Ceil, infer_types_ceil)

#' @title element-wise ceil operation
#' @param operand ([`FuncVariable`])
#' @return [`FuncVariable`]
#' @export
hlo_ceil <- function(operand) {
  hlo_ceil_impl(values = list(operand = operand))
}
