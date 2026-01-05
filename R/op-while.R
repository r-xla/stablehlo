#' @include op.R hlo.R
NULL

OpWhile <- new_Op("OpWhile", "while")

#' @rdname hlo_while
#' @export
infer_types_while <- function(..., cond, body) {
  value_types <- list(...)

  if (length(value_types) == 0L) {
    cli_abort("hlo_while must have at least one operand")
  }

  # (C1) cond has type (T0, ..., TN-1) -> tensor<i1>
  if (length(cond$outputs$items) != 1L) {
    cli_abort("cond must have exactly one output")
  }
  cond_out <- cond$outputs$items[[1L]]$type
  assert_vt_has_ttype(cond_out, "BooleanType")
  if (length(cond_out$type$shape$dims) != 0L) {
    cli_abort("cond output must be a 0-D tensor")
  }

  # (C2) body has type (T0, ..., TN-1) -> (T0, ..., TN-1)
  body_out_types <- func_output_types(body)
  if (length(body_out_types) != length(value_types)) {
    cli_abort("body must have the same number of outputs as inputs")
  }
  for (i in seq_along(value_types)) {
    assert_vt_equal(body_out_types[[i]], value_types[[i]])
  }

  # (C3) result types equal operand types
  ValueTypes(value_types)
}

hlo_while_impl <- hlo_fn(OpWhile, infer_types_while)

#' @templateVar mnemonic while
#' @templateVar not_func_variables simplify
#' @template op
#' @template param_simplify
#' @export
hlo_while <- function(..., cond, body, simplify = TRUE) {
  hlo_while_impl(
    values = list(...),
    funcs = list(cond = cond, body = body),
    simplify = simplify
  )
}
