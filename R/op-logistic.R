#' @include op.R hlo.R
NULL

OpLogistic <- new_Op("OpLogistic", "logistic")

infer_types_logistic <- function(operand) {
  stopifnot(inherits(operand@type, TensorType))
  ValueTypes(list(operand))
}
hlo_logistic_impl <- hlo_fn(OpLogistic, infer_types_logistic)

#' @templateVar mnemonic logistic
#' @template op
#' @export
hlo_logistic <- function(operand) {
  hlo_logistic_impl(values = list(operand = operand))
}
