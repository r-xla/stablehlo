#' @include op.R hlo.R
NULL

Add <- new_Op("Add", "add")

# binary ops
infer_types_add <- function(lhs, rhs) {
  stopifnot(inherits(lhs@type, TensorType))
  stopifnot(inherits(rhs@type, TensorType))
  #stopifnot(identical(lhs@type@dtype, rhs@type@dtype))
  stopifnot(lhs@type == rhs@type)

  ValueTypes(list(lhs))
}

hlo_add_impl <- hlo_fn(Add, infer_types_add)

#' @title Addition
#' @param lhs,rhs ([`FuncPointer`])
#' @return [`FuncPointer`]
#' @export
hlo_add <- function(lhs, rhs) {
  hlo_add_impl(values = list(lhs = lhs, rhs = rhs))
}
