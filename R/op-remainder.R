#' @include op.R hlo.R
NULL

OpRemainder <- new_Op("OpRemainder", "remainder")

infer_types_remainder <- function(lhs, rhs) {
  stopifnot(inherits(lhs@type, TensorType))
  stopifnot(inherits(rhs@type, TensorType))
  stopifnot(lhs@type == rhs@type)
  ValueTypes(list(lhs))
}
hlo_remainder_impl <- hlo_fn(OpRemainder, infer_types_remainder)

#' @templateVar mnemonic remainder
#' @template op
#' @export
hlo_remainder <- function(lhs, rhs) {
  hlo_remainder_impl(values = list(lhs = lhs, rhs = rhs))
}
