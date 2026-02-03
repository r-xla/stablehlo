#' @include op.R hlo.R
NULL

OpIf <- new_Op("OpIf", "if")

#' @rdname hlo_if
#' @export
infer_types_if <- function(pred, true_branch, false_branch) {
  assert_vt_has_ttype(pred, "BooleanType", shape = integer())
  out_types1 <- ValueTypes(func_output_types(true_branch))
  out_types2 <- ValueTypes(func_output_types(false_branch))
  if (length(out_types1) != length(out_types2)) {
    cli_abort(c(
      "{.arg true_branch} and {.arg false_branch} must have the same number of outputs.",
      x = "Got {length(out_types1)} and {length(out_types2)}."
    ))
  }
  for (i in seq_along(out_types1)) {
    if (out_types1[[i]] != out_types2[[i]]) {
      error_unequal_types(
        arg1 = "output_types(true_branch)",
        arg2 = "output_types(false_branch)",
        index = i - 1L,
        expected = "must have the same type",
        actual1 = out_types1[[i]],
        actual2 = out_types2[[i]]
      )
    }
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
