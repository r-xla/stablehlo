#' @include op.R api.R
NULL

Abs <- new_Op("Abs", "abs")

infer_types_abs <- function(operand) {
  stopifnot(inherits(operand@type, TensorType))
  ValueTypes(list(operand))
}

hlo_abs_impl <- hlo_fn(Abs, infer_types_abs)

#' @title Absolute value
#' @param operand [`FuncPointer`]
#' @return [`FuncPointer`]
#' @export
hlo_abs <- function(operand) {
  hlo_abs_impl(values = list(operand))
}
