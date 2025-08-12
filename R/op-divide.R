#' @include op.R hlo.R
NULL

Divide <- new_Op("Divide", "divide")

# binary ops
infer_types_divide <- function(lhs, rhs) {
  stopifnot(inherits(lhs@type, TensorType))
  stopifnot(inherits(rhs@type, TensorType))
  stopifnot(lhs@type == rhs@type)

  ValueTypes(list(lhs))
}

hlo_divide_impl <- hlo_fn(Divide, infer_types_divide)

#' @templateVar mnemonic divide
#' @template op
#' @export
hlo_divide <- function(lhs, rhs) {
  hlo_divide_impl(values = list(lhs = lhs, rhs = rhs))
}
