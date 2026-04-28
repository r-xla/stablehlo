#' @include op.R hlo.R type_inference.R
NULL

OpDigamma <- new_Op("OpDigamma", "digamma", dialect = "chlo")

#' @rdname hlo_digamma
#' @export
infer_types_digamma <- infer_types_float_uni

hlo_digamma_impl <- hlo_fn(OpDigamma, infer_types_digamma)

#' @templateVar mnemonic digamma
#' @template op
#' @export
hlo_digamma <- function(operand) {
  hlo_digamma_impl(values = list(operand = operand))
}
