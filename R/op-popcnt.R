#' @include op.R hlo.R type_inference.R
NULL

OpPopcnt <- new_Op("Popcnt", "popcnt")

infer_types_popcnt <- function(operand) {
  stopifnot(inherits(operand@type, TensorType))
  assert_one_of(operand@type@dtype, IntegerType)
  ValueTypes(list(operand))
}

hlo_popcnt_impl <- hlo_fn(OpPopcnt, infer_types_popcnt)

#' @templateVar mnemonic popcnt
#' @template op
#' @export
hlo_popcnt <- function(operand) {
  hlo_popcnt_impl(values = list(operand = operand))
}
