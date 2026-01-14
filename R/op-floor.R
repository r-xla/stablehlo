#' @include op.R hlo.R type_inference.R
NULL

OpFloor <- new_Op("OpFloor", "floor")

#' @rdname hlo_floor
#' @export
infer_types_floor <- infer_types_float_uni

hlo_floor_impl <- hlo_fn(OpFloor, infer_types_floor)

#' @templateVar mnemonic floor
#' @template op
#' @export
hlo_floor <- function(operand) {
  hlo_floor_impl(values = list(operand = operand))
}
