#' @include op.R hlo.R
NULL

OpIf <- new_Op("OpIf", "if")

#' @rdname hlo_if
#' @export
infer_types_if <- function(pred, true_branch, false_branch) {
  stopifnot(inherits(pred@type, TensorType))
  stopifnot(pred@type@dtype == BooleanType())
  stopifnot(length(pred@type@shape@dims) == 0)

  out_types1 <- ValueTypes(func_output_types(true_branch))
  out_types2 <- ValueTypes(func_output_types(false_branch))
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
