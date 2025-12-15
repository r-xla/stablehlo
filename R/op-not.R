#' @include op.R hlo.R type_inference.R
NULL

OpNot <- new_Op("OpNot", "not")

hlo_not_impl <- hlo_fn(OpNot, infer_types_integerish_uni)

#' @templateVar mnemonic not
#' @template op
#' @export
hlo_not <- function(operand) {
  hlo_not_impl(values = list(operand = operand))
}
