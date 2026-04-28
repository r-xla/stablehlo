#' @include op.R hlo.R type_inference.R
NULL

OpIsPosInf <- new_Op("OpIsPosInf", "is_pos_inf", dialect = "chlo")

#' @rdname hlo_is_pos_inf
#' @export
infer_types_is_pos_inf <- function(operand) {
  assert_vt_has_ttype(operand, "FloatType")
  ValueTypes(list(make_vt("pred", shape(operand))))
}

hlo_is_pos_inf_impl <- hlo_fn(OpIsPosInf, infer_types_is_pos_inf)

#' @templateVar mnemonic is_pos_inf
#' @template op_chlo
#' @export
hlo_is_pos_inf <- function(operand) {
  hlo_is_pos_inf_impl(values = list(operand = operand))
}
