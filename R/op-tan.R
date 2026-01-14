#' @include op.R hlo.R type_inference.R
NULL

OpTan <- new_Op("OpTan", "tan")

#' @rdname hlo_tan
#' @export
infer_types_tan <- infer_types_float_uni

hlo_tan_impl <- hlo_fn(OpTan, infer_types_tan)

#' @templateVar mnemonic tan
#' @template op
#' @export
hlo_tan <- function(operand) {
  hlo_tan_impl(values = list(operand = operand))
}
