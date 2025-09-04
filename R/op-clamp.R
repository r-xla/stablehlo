#' @include op.R hlo.R
NULL

OpClamp <- new_Op("OpClamp", "clamp")

# binary ops
infer_types_clamp <- function(Min, operand, Max) {
  stopifnot(inherits(Min@type, TensorType))
  stopifnot(inherits(Max@type, TensorType))
  stopifnot(inherits(operand@type, TensorType))
  stopifnot(operand@type == Max@type)
  stopifnot(Min@type == Max@type)
  ValueTypes(list(operand))
}

hlo_clamp_impl <- hlo_fn(OpClamp, infer_types_clamp)

#' @templateVar mnemonic clamp
#' @template op
#' @export
hlo_clamp <- function(Min, operand, Max) {
  hlo_clamp_impl(values = list(Min = Min, operand = operand, Max = Max))
}
