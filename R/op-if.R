#' @include op.R hlo.R
NULL

OpIf <- new_Op("OpIf", "if")

#' @rdname hlo_if
#' @export
infer_types_if <- function(pred, true_branch, false_branch) {
  assert_vt_is_tensor(
    pred,
    expected_dtypes = list(BooleanType),
    expected_shape = integer()
  )
  out_types1 <- ValueTypes(func_output_types(true_branch))
  out_types2 <- ValueTypes(func_output_types(false_branch))
  if (length(out_types1) != length(out_types2)) {
    cli_abort(
      "true_branch and false_branch must have the same number of outputs"
    )
  }
  for (i in seq_along(out_types1)) {
    assert_vt_equal(out_types1@items[[i]], out_types2@items[[i]])
  }
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
