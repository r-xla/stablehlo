#' @include op.R hlo.R type_inference.R
NULL

OpLogistic <- new_Op("OpLogistic", "logistic")

#' @rdname hlo_logistic
#' @export
infer_types_logistic <- infer_types_float_uni

hlo_logistic_impl <- hlo_fn(OpLogistic, infer_types_logistic)

#' @templateVar mnemonic logistic
#' @template op
#' @export
hlo_logistic <- function(operand, output_types = NULL) {
  hlo_logistic_impl(
    values = list(operand = operand),
    output_types = output_types
  )
}
