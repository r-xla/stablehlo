#' @include op.R hlo.R type_inference.R 
NULL 

OpRsqrt <- new_Op("OpRsqrt", "rsqrt")

hlo_rsqrt_impl <- hlo_fn(OpRsqrt, infer_types_generic_uni) 

#' @templateVar mnemonic rsqrt
#' @template op
#' @export
hlo_rsqrt <- function(operand) {
  hlo_rsqrt_impl(values = list(operand = operand))
}
