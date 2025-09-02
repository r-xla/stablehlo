#' @include op.R hlo.R
NULL

Subtract <- new_Op("Subtract", "subtract")

infer_types_subtract <- function(lhs, rhs) {
  stopifnot(inherits(lhs@type, TensorType))
  stopifnot(inherits(rhs@type, TensorType))
  stopifnot(lhs@type == rhs@type)
  ValueTypes(list(lhs))
}
hlo_subtract_impl <- hlo_fn(Subtract, infer_types_subtract)

#' @templateVar mnemonic subtract
#' @template op
#' @export
hlo_subtract <- function(lhs, rhs) {
  hlo_subtract_impl(values = list(lhs = lhs, rhs = rhs))
}
