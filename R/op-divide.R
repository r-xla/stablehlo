#' @include op.R hlo.R type_inference.R
NULL

OpDivide <- new_Op("OpDivide", "divide")

#' @rdname hlo_divide
#' @export
infer_types_divide <- infer_types_numeric_biv

hlo_divide_impl <- hlo_fn(OpDivide, infer_types_divide)

#' @templateVar mnemonic divide
#' @template op
#' @export
hlo_divide <- function(lhs, rhs, output_types = NULL) {
  hlo_divide_impl(
    values = list(lhs = lhs, rhs = rhs),
    output_types = output_types
  )
}
