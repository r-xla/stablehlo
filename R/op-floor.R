#' @include op.R hlo.R utils.R 
NULL 

OpFloor <- new_Op("OpFloor", "floor")

hlo_floor_impl <- hlo_fn(OpFloor, infer_types_generic_uni) 

#' @templateVar mnemonic floor
#' @template op
#' @export
hlo_floor <- function(operand) {
  hlo_floor_impl(values = list(operand = operand))
}
