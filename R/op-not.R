#' @include op.R hlo.R type_inference.R
NULL

OpNot <- new_Op("OpNot", "not")

#' @rdname hlo_not
#' @export
infer_types_not <- infer_types_integerish_uni

hlo_not_impl <- hlo_fn(OpNot, infer_types_not)

#' @templateVar mnemonic not
#' @template op
#' @export
hlo_not <- function(operand) {
  hlo_not_impl(values = list(operand = operand))
}
