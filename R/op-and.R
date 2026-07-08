#' @include op.R hlo.R type_inference.R
NULL

OpAnd <- new_Op("OpAnd", "and")

#' @rdname hlo_and
#' @export
infer_types_and <- infer_types_integerish_biv

hlo_and_impl <- hlo_fn(OpAnd, infer_types_and)

#' @templateVar mnemonic and
#' @template op
#' @export
hlo_and <- function(lhs, rhs, output_types = NULL) {
  hlo_and_impl(values = list(lhs = lhs, rhs = rhs), output_types = output_types)
}
