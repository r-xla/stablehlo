#' @include op.R api.R
NULL

If <- new_Op("If", "if")

infer_types_if <- function(pred, .funcs) {
  # Check that pred is a 0-dimensional tensor of type i1 (boolean)
  stopifnot(inherits(pred@type, TensorType))
  stopifnot(pred@type@dtype@type == BooleanType())
  stopifnot(length(pred@type@shape@dims) == 0)  # 0-dimensional tensor

  out_types1 <- ValueTypes(lapply(.funcs[[1]]@outputs@items, function(x) x@type))
  out_types2 <- ValueTypes(lapply(.funcs[[2]]@outputs@items, function(x) x@type))
  stopifnot(out_types1 == out_types2)
  out_types1
}
