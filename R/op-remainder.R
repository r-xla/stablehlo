#' @include op.R hlo.R utils.R 
NULL 

OpRemainder <- new_Op("OpRemainder", "remainder")

hlo_remainder_impl <- hlo_fn(OpRemainder, infer_types_generic_biv) 

#' @templateVar mnemonic remainder
#' @template op
#' @export
hlo_remainder <- function(lhs, rhs) {
  hlo_remainder_impl(values = list(lhs = lhs, rhs = rhs))
}
