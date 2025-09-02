#' @include op.R hlo.R
NULL

OpAtan2 <- new_Op("OpAtan2", "atan2")

infer_types_atan2 <- function(lhs, rhs) {
  stopifnot(inherits(lhs@type, TensorType))
  stopifnot(inherits(rhs@type, TensorType))
  stopifnot(lhs@type == rhs@type)
  ValueTypes(list(lhs))
}
hlo_atan2_impl <- hlo_fn(OpAtan2, infer_types_atan2)

#' @templateVar mnemonic atan2
#' @template op
#' @export
hlo_atan2 <- function(lhs, rhs) {
  hlo_atan2_impl(values = list(lhs = lhs, rhs = rhs))
}
