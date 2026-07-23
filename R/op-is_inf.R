#' @include op.R hlo.R type_inference.R
NULL

OpIsInf <- new_Op("OpIsInf", "is_inf", dialect = "chlo")

#' @rdname hlo_is_inf
#' @export
infer_types_is_inf <- function(operand) {
  assert_vt_has_ttype(operand, "float")
  ValueTypes(list(make_vt("pred", shape(operand))))
}

hlo_is_inf_impl <- hlo_fn(OpIsInf, infer_types_is_inf)

#' @templateVar mnemonic is_inf
#' @template op_chlo
#' @export
hlo_is_inf <- function(operand, output_types = NULL) {
  hlo_is_inf_impl(values = list(operand = operand), output_types = output_types)
}
