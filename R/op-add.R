#' @include op.R hlo.R
NULL

Add <- new_Op("Add", "add")

infer_types_add <- function(lhs, rhs) {
  stopifnot(inherits(lhs@type, TensorType))
  stopifnot(inherits(rhs@type, TensorType))
  stopifnot(lhs@type == rhs@type)

  ValueTypes(list(lhs))
}

hlo_add_impl <- hlo_fn(Add, infer_types_add)

#' @template op
#' @templateVar mnemonic add
#' @export
hlo_add <- function(lhs, rhs) {
  hlo_add_impl(values = list(lhs = lhs, rhs = rhs))
}
