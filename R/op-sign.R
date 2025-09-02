#' @include op.R hlo.R
NULL

Sign <- new_Op("Sign", "sign")

infer_types_sign <- function(operand) {
  stopifnot(inherits(operand@type, TensorType))
  ValueTypes(list(operand))
}
hlo_sign_impl <- hlo_fn(Sign, infer_types_sign)

#' @templateVar mnemonic sign
#' @template op
#' @export
hlo_sign <- function(operand) {
  hlo_sign_impl(values = list(operand = operand))
}
