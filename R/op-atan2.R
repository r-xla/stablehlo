#' @include op.R hlo.R
NULL

Atan2 <- new_Op("Atan2", "atan2")

infer_types_atan2 <- function(lhs, rhs) {
  stopifnot(inherits(lhs@type, TensorType))
  stopifnot(lhs@type == rhs@type)
  stopifnot(lhs@type@dtype == rhs@type@dtype)
  assert_one_of(lhs@type@dtype@type, FloatType, ComplexType)
  ValueTypes(list(lhs))
}

hlo_atan2_impl <- hlo_fn(Atan2, infer_types_atan2)

#' @templateVar mnemonic atan2
#' @template op
#' @export
hlo_atan2 <- function(lhs, rhs) {
  hlo_atan2_impl(values = list(lhs = lhs, rhs = rhs))
}
