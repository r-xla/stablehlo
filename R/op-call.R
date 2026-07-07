#' @include op.R hlo.R
NULL


#' @rdname hlo_call
#' @export
infer_types_call <- function(callee, ...) {
  args <- list(...)
  callee_inputs <- callee$inputs
  callee_outputs <- callee$outputs

  if (length(args) != length(callee_inputs)) {
    cli_abort(c(
      "Number of arguments must match the callee's input count.",
      x = "Expected {length(callee_inputs)} argument{?s}, got {length(args)}."
    ))
  }

  for (i in seq_along(args)) {
    expected <- callee_inputs[[i]]$type
    got <- args[[i]]
    if (expected != got) {
      cli_abort(c(
        "Argument {i} type must match the callee's input type.",
        x = "Expected {.val {expected}}, got {.val {got}}."
      ))
    }
  }

  ValueTypes(lapply(callee_outputs, function(o) o$type))
}

#' @title Call a Function
#' @description
#' Calls a named function from within the current function being built.
#' The callee must already be finalized via [`hlo_return`].
#' @param callee ([`Func`])\cr
#'   The function to call. Must be a finalized [`Func`].
#' @param ... ([`FuncValue`])\cr
#'   The arguments to pass to the callee.
#' @param simplify (`logical(1)`)\cr
#'   If `TRUE` (default) and the callee has a single output, return a single
#'   [`FuncValue`] instead of a list.
#' @return [`FuncValue`] or `list()` of [`FuncValue`]s.
#' @export
hlo_call <- function(callee, ..., simplify = TRUE) {
  if (!test_class(callee, "Func")) {
    cli_abort("{.arg callee} must be a {.cls Func} object.")
  }
  if (length(callee$outputs) == 0L) {
    cli_abort("{.arg callee} must be finalized via {.fn hlo_return}.")
  }

  dots <- list(...)
  lapply(dots, function(x) {
    if (!test_class(x, "FuncValue")) {
      cli_abort("All arguments must be {.cls FuncValue}s.")
    }
  })

  func <- merge_funcs(lapply(dots, function(x) x$func))

  input_types <- lapply(dots, function(x) x$value_type)

  output_types <- do.call(infer_types_call, c(list(callee), input_types))

  nout <- length(output_types)
  output_value_ids <- lapply(seq_len(nout), function(i) ValueId())

  func_emit(
    func,
    list(
      function(p) {
        paste0(
          ids_str(p$output_ids),
          " = func.call ",
          p$callee_id,
          "(",
          ids_str(p$value_ids),
          ") : (",
          p$in_types,
          ") -> (",
          p$out_types,
          ")"
        )
      },
      list(
        output_ids = output_value_ids,
        value_ids = lapply(dots, function(x) x$value_id),
        callee_id = repr(callee$id),
        in_types = paste0(
          vapply(input_types, type_str, character(1)),
          collapse = ", "
        ),
        out_types = paste0(
          vapply(output_types, type_str, character(1)),
          collapse = ", "
        )
      )
    )
  )

  if (nout == 1L && simplify) {
    return(
      FuncValue(
        value_id = output_value_ids[[1L]],
        value_type = output_types[[1L]],
        func = func
      )
    )
  }
  lapply(seq_len(nout), function(i) {
    FuncValue(
      value_id = output_value_ids[[i]],
      value_type = output_types[[i]],
      func = func
    )
  })
}
