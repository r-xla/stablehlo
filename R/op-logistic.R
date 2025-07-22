#' @include op.R hlo.R type_inference.R
NULL

OpLogistic <- new_Op("OpLogistic", "logistic")

hlo_logistic_impl <- hlo_fn(OpLogistic, infer_types_generic_uni)

#' @templateVar mnemonic logistic
#' @templateVar params %s
#' @templateVar attrs %s
#' @template op
#' @export
hlo_logistic <- function(operand) {
  hlo_logistic_impl(values = list(operand = operand))
}
