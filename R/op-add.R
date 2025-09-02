#' @include op.R hlo.R
NULL

OpAdd <- new_Op("OpAdd", "add")

infer_types_add <- function(lhs, rhs) {
  stopifnot(inherits(lhs@type, TensorType))
  stopifnot(inherits(rhs@type, TensorType))
  stopifnot(lhs@type == rhs@type)
  ValueTypes(list(lhs))
}
hlo_add_impl <- hlo_fn(OpAdd, infer_types_add)

#' @templateVar mnemonic add
#' @template op
#' @export
hlo_add <- function(lhs, rhs) {
  hlo_add_impl(values = list(lhs = lhs, rhs = rhs))
}
