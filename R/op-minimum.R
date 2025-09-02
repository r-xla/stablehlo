#' @include op.R hlo.R 
NULL 

Minimum <- new_Op("Minimum", "minimum")

infer_types_minimum <- function(lhs, rhs) {
  stopifnot(inherits(lhs@type, TensorType))
  stopifnot(inherits(rhs@type, TensorType))
  stopifnot(lhs@type == rhs@type)
  ValueTypes(list(lhs))
}
hlo_minimum_impl <- hlo_fn(Minimum, infer_types_minimum) 

#' @templateVar mnemonic minimum
#' @template op
#' @export
hlo_minimum <- function(lhs, rhs) {
  hlo_minimum_impl(values = list(lhs = lhs, rhs = rhs))
}
