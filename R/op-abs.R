#' @include op.R hlo.R type_inference.R utils.R
NULL

OpAbs <- new_Op("OpAbs", "abs")


#' @rdname hlo_abs
#' @export
infer_types_abs <- function(operand) {
  assert_vt_has_ttype(operand, "FloatType", "IntegerType")
  ValueTypes(list(operand))
}

hlo_abs_impl <- hlo_fn(OpAbs, infer_types_numeric_uni)

#' @templateVar mnemonic abs
#' @template op
#' @export
hlo_abs <- function(operand) {
  hlo_abs_impl(values = list(operand = operand))
}
