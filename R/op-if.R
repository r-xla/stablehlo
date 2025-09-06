#' @include op.R hlo.R
NULL

OpIf <- new_Op("OpIf", "if")

infer_types_if <- function(pred, true_branch, false_branch) {
  stopifnot(inherits(pred@type, TensorType))
  stopifnot(pred@type@dtype == BooleanType())
  stopifnot(length(pred@type@shape@dims) == 0)

  out_types1 <- ValueTypes(
    lapply(true_branch@outputs@items, function(x) {
      x@type
    })
  )
  out_types2 <- ValueTypes(
    lapply(false_branch@outputs@items, function(x) {
      x@type
    })
  )
  stopifnot(out_types1 == out_types2)
  out_types1
}

hlo_if_impl <- hlo_fn(OpIf, infer_types_if)

#' @templateVar mnemonic if
#' @template op
#' @export
hlo_if <- function(pred, true_branch, false_branch) {
  hlo_if_impl(
    values = list(pred = pred),
    funcs = list(
      true_branch = true_branch,
      false_branch = false_branch
    )
  )
}
