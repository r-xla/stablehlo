#' @include op.R hlo.R
NULL

If <- new_Op("If", "if")

infer_types_if <- function(pred, true_branch, false_branch) {
  # Check that pred is a 0-dimensional tensor of type i1 (boolean)
  stopifnot(inherits(pred@type, TensorType))
  stopifnot(pred@type@dtype@type == BooleanType())
  stopifnot(length(pred@type@shape@dims) == 0) # 0-dimensional tensor

  out_types1 <- ValueTypes(lapply(true_branch@outputs@items, function(x) {
    x@type
  }))
  out_types2 <- ValueTypes(lapply(false_branch@outputs@items, function(x) {
    x@type
  }))
  stopifnot(out_types1 == out_types2)
  out_types1
}

hlo_if_impl <- hlo_fn(If, infer_types_if)

hlo_if <- function(pred, true_branch, false_branch) {
  hlo_if_impl(
    values = list(pred = pred),
    funcs = list(
      true_branch = true_branch,
      false_branch = false_branch
    )
  )
}

method(repr, If) <- function(x, toplevel = TRUE) {
  paste0(
    repr(x@outputs),
    repr(x@name),
    repr(x@inputs@values),
    repr(x@inputs@funcs),
    ":",
    repr(x@signature)
  )
}
