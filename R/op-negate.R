#' @include op.R hlo.R type_inference.R
NULL

OpNegate <- new_Op("OpNegate", "negate")

#' @rdname hlo_negate
#' @export
infer_types_negate <- infer_types_numeric_uni

hlo_negate_impl <- hlo_fn(OpNegate, infer_types_negate)

#' @templateVar mnemonic negate
#' @template op
#' @export
hlo_negate <- function(operand) {
  hlo_negate_impl(values = list(operand = operand))
}
