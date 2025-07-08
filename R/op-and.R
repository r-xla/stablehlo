#' @include op.R hlo.R
NULL

And <- new_Op("And", "and")

infer_types_and <- function(lhs, rhs) {
  stopifnot(inherits(lhs@type, TensorType))
  stopifnot(lhs@type == rhs@type)
  assert_one_of(lhs@type@dtype@type, IntegerType, BooleanType)

  ValueTypes(list(lhs))
}

hlo_and_impl <- hlo_fn(And, infer_types_and)

#' @title And
#'
#' @description
#' Logical AND operation.
#'
#' @param lhs,rhs ([`FuncPointer`])
#'
#' @return [`FuncPointer`]
#' @export
hlo_and <- function(lhs, rhs) {
  hlo_and_impl(values = list(lhs = lhs, rhs = rhs))
}
