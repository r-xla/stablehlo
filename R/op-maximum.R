#' @include op.R hlo.R type_inference.R 
NULL 

OpMaximum <- new_Op("OpMaximum", "maximum")

hlo_maximum_impl <- hlo_fn(OpMaximum, infer_types_generic_biv) 

#' @templateVar mnemonic maximum
#' @template op
#' @export
hlo_maximum <- function(lhs, rhs) {
  hlo_maximum_impl(values = list(lhs = lhs, rhs = rhs))
}
