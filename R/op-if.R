#' @include op.R hlo.R
NULL

OpIf <- new_Op("OpIf", "if")

#' @rdname hlo_if
#' @export
infer_types_if <- function(pred, true_branch, false_branch) {
  assert_vt_is_tensor(pred)
  assert_vt_has_dtype(pred, BooleanType)
  stopifnot(length(pred@type@shape@dims) == 0)

  out_types1 <- ValueTypes(func_output_types(true_branch))
  out_types2 <- ValueTypes(func_output_types(false_branch))
  assert_vt_equal(out_types1, out_types2)
  out_types1
}

hlo_if_impl <- hlo_fn(OpIf, infer_types_if)

#' @templateVar mnemonic if
#' @templateVar not_func_variables simplify
#' @template op
#' @template param_simplify
#' @export
hlo_if <- function(pred, true_branch, false_branch, simplify = TRUE) {
  hlo_if_impl(
    values = list(pred = pred),
    funcs = list(
      true_branch = true_branch,
      false_branch = false_branch
    ),
    simplify = simplify
  )
}
