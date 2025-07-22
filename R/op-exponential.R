#' @include op.R hlo.R type_inference.R
NULL

OpExponential <- new_Op("OpExponential", "exponential")

hlo_exponential_impl <- hlo_fn(OpExponential, infer_types_generic_uni)

#' @templateVar mnemonic exponential
#' @templateVar params %s
#' @templateVar attrs %s
#' @template op
#' @export
hlo_exponential <- function(operand) {
  hlo_exponential_impl(values = list(operand = operand))
}
