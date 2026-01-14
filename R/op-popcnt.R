#' @include op.R hlo.R type_inference.R
NULL

OpPopcnt <- new_Op("Popcnt", "popcnt")

#' @rdname hlo_popcnt
#' @export
infer_types_popcnt <- infer_types_integer_uni

hlo_popcnt_impl <- hlo_fn(OpPopcnt, infer_types_popcnt)

#' @templateVar mnemonic popcnt
#' @template op
#' @export
hlo_popcnt <- function(operand) {
  hlo_popcnt_impl(values = list(operand = operand))
}
