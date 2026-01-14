#' @include op.R hlo.R type_inference.R
NULL

OpPopcnt <- new_Op("Popcnt", "popcnt")

hlo_popcnt_impl <- hlo_fn(OpPopcnt, infer_types_integer_uni)

#' @templateVar mnemonic popcnt
#' @template op
#' @export
hlo_popcnt <- function(operand) {
  hlo_popcnt_impl(values = list(operand = operand))
}
