#' @include op.R hlo.R
NULL

Power <- new_Op("Power", "power")

infer_types_power <- function(lhs, rhs) {
  stopifnot(inherits(lhs@type, TensorType))
  stopifnot(inherits(rhs@type, TensorType))
  stopifnot(lhs@type == rhs@type)
  ValueTypes(list(lhs))
}
hlo_power_impl <- hlo_fn(Power, infer_types_power)

#' @templateVar mnemonic power
#' @template op
#' @export
hlo_power <- function(lhs, rhs) {
  hlo_power_impl(values = list(lhs = lhs, rhs = rhs))
}
