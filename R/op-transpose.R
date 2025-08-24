#' @include op.R hlo.R
NULL

Transpose <- new_Op("Transpose", "transpose")

# binary ops
infer_types_transpose <- function(operand, permutation) {
  # TODO
  # stopifnot(inherits(operand@type, TensorType))
  # make_value_type(operand@type, operand@shape[permutation])
  # ValueTypes(list(operand))
}

hlo_transpose_impl <- hlo_fn(Transpose, infer_types_transpose)

#' @title Transpose
#' @param operand ([`FuncVariable`])
#' @return [`FuncVariable`]
#' @export
hlo_transpose <- function(operand, permutation) {
  hlo_transpose_impl(values = list(operand = operand, permutation = permutation))
}
