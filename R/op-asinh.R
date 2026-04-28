#' @include op.R hlo.R type_inference.R
NULL

OpAsinh <- new_Op("OpAsinh", "asinh", dialect = "chlo")

#' @rdname hlo_asinh
#' @export
infer_types_asinh <- infer_types_float_uni

hlo_asinh_impl <- hlo_fn(OpAsinh, infer_types_asinh)

#' @templateVar mnemonic asinh
#' @template op
#' @export
hlo_asinh <- function(operand) {
  hlo_asinh_impl(values = list(operand = operand))
}
