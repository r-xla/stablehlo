#' @include op.R api.R
NULL

Abs <- new_Op("Abs", "abs")

infer_types_abs <- function(operand) {
  stopifnot(inherits(operand@type, TensorType))
  ValueTypes(list(operand))
}

hlo_abs <- hlo_fn(Abs, infer_types_abs)
