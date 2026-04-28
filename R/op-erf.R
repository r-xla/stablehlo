#' @include op.R hlo.R type_inference.R
NULL

OpErf <- new_Op("OpErf", "erf", dialect = "chlo")

#' @rdname hlo_erf
#' @export
infer_types_erf <- infer_types_float_uni

hlo_erf_impl <- hlo_fn(OpErf, infer_types_erf)

#' @templateVar mnemonic erf
#' @template op_chlo
#' @export
hlo_erf <- function(operand) {
  hlo_erf_impl(values = list(operand = operand))
}
