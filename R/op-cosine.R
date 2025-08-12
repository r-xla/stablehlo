#' @include op.R hlo.R
NULL

Cosine <- new_Op("Cosine", "cosine")

# binary ops
infer_types_cosine <- function(operand) {
  stopifnot(inherits(operand@type, TensorType))
  ValueTypes(list(operand))
}

hlo_cosine_impl <- hlo_fn(Cosine, infer_types_cosine)

#' @templateVar mnemonic cosine
#' @template op
#' @export
hlo_cosine <- function(operand) {
  hlo_cosine_impl(values = list(operand = operand))
}
