#' @include op.R hlo.R
NULL

OpMaximum <- new_Op("OpMaximum", "maximum")

infer_types_maximum <- function(lhs, rhs) {
  stopifnot(inherits(lhs@type, TensorType))
  stopifnot(inherits(rhs@type, TensorType))
  stopifnot(lhs@type == rhs@type)
  ValueTypes(list(lhs))
}
hlo_maximum_impl <- hlo_fn(OpMaximum, infer_types_maximum)

#' @templateVar mnemonic maximum
#' @template op
#' @export
hlo_maximum <- function(lhs, rhs) {
  hlo_maximum_impl(values = list(lhs = lhs, rhs = rhs))
}
