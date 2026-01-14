#' @include op.R hlo.R type_inference.R
NULL

OpSign <- new_Op("OpSign", "sign")

#' @rdname hlo_sign
#' @export
infer_types_sign <- function(operand) {
  assert_vt_is_tensor(operand)
  assert_vt_has_ttype(operand, "FloatType", "IntegerType")
  ValueTypes(list(operand))
}

hlo_sign_impl <- hlo_fn(OpSign, infer_types_sign)

#' @templateVar mnemonic sign
#' @template op
#' @export
hlo_sign <- function(operand) {
  hlo_sign_impl(values = list(operand = operand))
}
