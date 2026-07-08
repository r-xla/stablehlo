#' @include op.R hlo.R type_inference.R
NULL

OpErfInv <- new_Op("OpErfInv", "erf_inv", dialect = "chlo")

#' @rdname hlo_erf_inv
#' @export
infer_types_erf_inv <- infer_types_float_uni

hlo_erf_inv_impl <- hlo_fn(OpErfInv, infer_types_erf_inv)

#' @templateVar mnemonic erf_inv
#' @template op_chlo
#' @export
hlo_erf_inv <- function(operand, output_types = NULL) {
  hlo_erf_inv_impl(
    values = list(operand = operand),
    output_types = output_types
  )
}
