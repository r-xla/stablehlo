#' @include op.R hlo.R utils.R
NULL

OpCosine <- new_Op("OpCosine", "cosine")

hlo_cosine_impl <- hlo_fn(OpCosine, infer_types_generic_uni)

#' @templateVar mnemonic cosine
#' @template op
#' @export
hlo_cosine <- function(operand) {
  hlo_cosine_impl(values = list(operand = operand))
}
