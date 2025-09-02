#' @include op.R hlo.R
NULL

OpSign <- new_Op("OpSign", "sign")

infer_types_sign <- function(operand) {
  stopifnot(inherits(operand@type, TensorType))
  ValueTypes(list(operand))
}
hlo_sign_impl <- hlo_fn(OpSign, infer_types_sign)

#' @templateVar mnemonic sign
#' @template op
#' @export
hlo_sign <- function(operand) {
  hlo_sign_impl(values = list(operand = operand))
}
