#' @include op.R api.R
NULL

Atan2 <- new_Op("Atan2", "atan2")

infer_types_atan2 <- function(lhs, rhs) {
  stopifnot(inherits(lhs@type, TensorType))
  stopifnot(lhs@type == rhs@type)
  stopifnot(lhs@type@dtype == rhs@type@dtype)
  assert_one_of(lhs@type@dtype@type, FloatType, ComplexType)
  ValueTypes(list(lhs))
}

.hlo_atan2 <- hlo_fn(Atan2, infer_types_atan2)

hlo_atan2 <- function(lhs, rhs) {
  .hlo_atan2(values = list(lhs, rhs))
}
