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

  # (C1)
  if (length(cond$outputs) != 1L) {
    cli_abort("cond must have exactly one output")
  }
  if (length(cond$inputs) != length(value_types)) {
    cli_abort(c(
      "cond must have the same number of inputs as inputs",
      i = "Got {length(cond$inputs)} inputs and {length(value_types)} inputs."
    ))
  }
  cond_in_types <- lapply(cond$inputs, function(x) x$type)
  for (i in seq_along(value_types)) {
    if (cond_in_types[[i]] != value_types[[i]]) {
      error_unequal_types(
        arg1 = "cond input",
        arg2 = "input",
        index = i - 1L,
        expected = "must have the same type",
        actual1 = repr(cond_in_types[[i]]),
        actual2 = repr(value_types[[i]])
      )
    }
  }

  cond_out <- cond$outputs[[1L]]$type
  assert_vt_has_ttype(
    cond_out,
    "BooleanType",
    shape = integer(),
    arg = "output(condition)"
  )

  # (C2)
  body_out_types <- func_output_types(body)
  if (length(body_out_types) != length(value_types)) {
    cli_abort(c(
      "body must have the same number of outputs as inputs",
      i = "Got {length(body_out_types)} outputs and {length(value_types)} inputs."
    ))
  }
  for (i in seq_along(value_types)) {
    if (body_out_types[[i]] != value_types[[i]]) {
      error_unequal_types(
        arg1 = "body output",
        arg2 = "input",
        index = i - 1L, # 0-based
        expected = "must have the same type",
        actual1 = repr(body_out_types[[i]]),
        actual2 = repr(value_types[[i]])
      )
    }
  }

  # (C3)
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
