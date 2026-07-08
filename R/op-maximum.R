#' @include op.R hlo.R type_inference.R
NULL

OpMaximum <- new_Op("OpMaximum", "maximum")

#' @rdname hlo_maximum
#' @export
infer_types_maximum <- infer_types_generic_biv

hlo_maximum_impl <- hlo_fn(OpMaximum, infer_types_maximum)

#' @templateVar mnemonic maximum
#' @template op
#' @export
hlo_maximum <- function(lhs, rhs, output_types = NULL) {
  hlo_maximum_impl(
    values = list(lhs = lhs, rhs = rhs),
    output_types = output_types
  )
}
