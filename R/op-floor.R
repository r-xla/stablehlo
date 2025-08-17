#' @include op.R hlo.R
NULL

Floor <- new_Op("Floor", "floor")

infer_types_floor <- function(operand) {
  stopifnot(inherits(operand@type, TensorType))
  ValueTypes(list(operand))
}

hlo_floor_impl <- hlo_fn(Floor, infer_types_floor)

#' @templateVar mnemonic floor
#' @template op
#' @export
hlo_floor <- function(operand) {
  hlo_floor_impl(values = list(operand = operand))
}
