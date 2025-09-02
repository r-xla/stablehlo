#' @include op.R hlo.R
NULL

OpMinimum <- new_Op("OpMinimum", "minimum")

infer_types_minimum <- function(lhs, rhs) {
  stopifnot(inherits(lhs@type, TensorType))
  stopifnot(inherits(rhs@type, TensorType))
  stopifnot(lhs@type == rhs@type)
  ValueTypes(list(lhs))
}
hlo_minimum_impl <- hlo_fn(OpMinimum, infer_types_minimum)

#' @templateVar mnemonic minimum
#' @template op
#' @export
hlo_minimum <- function(lhs, rhs) {
  hlo_minimum_impl(values = list(lhs = lhs, rhs = rhs))
}
