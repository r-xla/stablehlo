#' @include op.R hlo.R type_inference.R
NULL

OpSquare <- new_Op("OpSquare", "square", dialect = "chlo")

#' @rdname hlo_square
#' @export
infer_types_square <- infer_types_float_uni

hlo_square_impl <- hlo_fn(OpSquare, infer_types_square)

#' @templateVar mnemonic square
#' @template op_chlo
#' @export
hlo_square <- function(operand) {
  hlo_square_impl(values = list(operand = operand))
}
