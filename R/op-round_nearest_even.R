#' @include op.R hlo.R 
NULL 

RoundNearestEven <- new_Op("RoundNearestEven", "round_nearest_even")

infer_types_round_nearest_even <- function(operand) {
  stopifnot(inherits(operand@type, TensorType))
  ValueTypes(list(operand))
}
hlo_round_nearest_even_impl <- hlo_fn(RoundNearestEven, infer_types_round_nearest_even) 

#' @templateVar mnemonic round_nearest_even
#' @template op
#' @export
hlo_round_nearest_even <- function(operand) {
  hlo_round_nearest_even_impl(values = list(operand = operand))
}
