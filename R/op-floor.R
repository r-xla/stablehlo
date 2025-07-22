#' @include op.R hlo.R type_inference.R
NULL

OpFloor <- new_Op("OpFloor", "floor")

hlo_floor_impl <- hlo_fn(OpFloor, infer_types_generic_uni)

#' @templateVar mnemonic floor
#' @templateVar params %s
#' @templateVar attrs %s
#' @template op
#' @export
hlo_floor <- function(operand) {
  hlo_floor_impl(values = list(operand = operand))
}
