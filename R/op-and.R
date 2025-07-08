#' @include op.R api.R
NULL

And <- new_Op("And", "and")

infer_types_and <- function(lhs, rhs) {
  stopifnot(inherits(lhs@type, TensorType))
  stopifnot(lhs@type == rhs@type)
  assert_one_of(lhs@type@dtype@type, IntegerType, BooleanType)

  ValueTypes(list(lhs))
}

hlo_and <- hlo_fn(And, infer_types_and)
