#' @include op.R hlo.R type_inference.R
NULL

OpCosh <- new_Op("OpCosh", "cosh", dialect = "chlo")

#' @rdname hlo_cosh
#' @export
infer_types_cosh <- infer_types_float_uni

hlo_cosh_impl <- hlo_fn(OpCosh, infer_types_cosh)

#' @templateVar mnemonic cosh
#' @template op
#' @export
hlo_cosh <- function(operand) {
  hlo_cosh_impl(values = list(operand = operand))
}
