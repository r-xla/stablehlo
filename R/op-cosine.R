#' @include op.R hlo.R type_inference.R
NULL

OpCosine <- new_Op("OpCosine", "cosine")

#' @rdname hlo_cosine
#' @export
infer_types_cosine <- infer_types_float_uni

hlo_cosine_impl <- hlo_fn(OpCosine, infer_types_cosine)

#' @templateVar mnemonic cosine
#' @template op
#' @export
hlo_cosine <- function(operand) {
  hlo_cosine_impl(values = list(operand = operand))
}
