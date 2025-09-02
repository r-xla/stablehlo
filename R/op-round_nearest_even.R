#' @include op.R hlo.R utils.R 
NULL 

OpRoundNearestEven <- new_Op("OpRoundNearestEven", "round_nearest_even")

hlo_round_nearest_even_impl <- hlo_fn(OpRoundNearestEven, infer_types_generic_uni) 

#' @templateVar mnemonic round_nearest_even
#' @template op
#' @export
hlo_round_nearest_even <- function(operand) {
  hlo_round_nearest_even_impl(values = list(operand = operand))
}
