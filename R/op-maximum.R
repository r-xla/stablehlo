#' @include op.R hlo.R 
NULL 

Maximum <- new_Op("Maximum", "maximum")

infer_types_maximum <- function(lhs, rhs) {
  stopifnot(inherits(lhs@type, TensorType))
  stopifnot(inherits(rhs@type, TensorType))
  stopifnot(lhs@type == rhs@type)
  ValueTypes(list(lhs))
}
hlo_maximum_impl <- hlo_fn(Maximum, infer_types_maximum) 

#' @templateVar mnemonic maximum
#' @template op
#' @export
hlo_maximum <- function(lhs, rhs) {
  hlo_maximum_impl(values = list(lhs = lhs, rhs = rhs))
}
