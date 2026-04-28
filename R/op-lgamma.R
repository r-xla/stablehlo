#' @include op.R hlo.R type_inference.R
NULL

OpLgamma <- new_Op("OpLgamma", "lgamma", dialect = "chlo")

#' @rdname hlo_lgamma
#' @export
infer_types_lgamma <- infer_types_float_uni

hlo_lgamma_impl <- hlo_fn(OpLgamma, infer_types_lgamma)

#' @templateVar mnemonic lgamma
#' @template op_chlo
#' @export
hlo_lgamma <- function(operand) {
  hlo_lgamma_impl(values = list(operand = operand))
}
