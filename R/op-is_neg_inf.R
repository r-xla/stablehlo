#' @include op.R hlo.R type_inference.R
NULL

OpIsNegInf <- new_Op("OpIsNegInf", "is_neg_inf", dialect = "chlo")

#' @rdname hlo_is_neg_inf
#' @export
infer_types_is_neg_inf <- function(operand) {
  assert_vt_has_ttype(operand, "FloatType")
  ValueTypes(list(make_vt("pred", shape(operand))))
}

hlo_is_neg_inf_impl <- hlo_fn(OpIsNegInf, infer_types_is_neg_inf)

#' @templateVar mnemonic is_neg_inf
#' @template op_chlo
#' @export
hlo_is_neg_inf <- function(operand) {
  hlo_is_neg_inf_impl(values = list(operand = operand))
}
