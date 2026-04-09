#' @include op.R hlo.R
NULL

OpCall <- function(inputs, outputs, signature) {
  base_op <- Op(
    name = OpName("call"),
    inputs = inputs,
    outputs = outputs,
    signature = signature
  )
  class(base_op) <- c("OpCall", "Op")
  base_op
}

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

  input_value_ids <- lapply(dots, function(x) OpInputValue(x$value_id))
  input_types <- lapply(dots, function(x) x$value_type)

  output_types <- rlang::exec(infer_types_call, callee, !!!input_types)

  nout <- length(output_types)
  output_value_ids <- replicate(nout, ValueId(), simplify = FALSE)
  outputs <- OpOutputs(lapply(output_value_ids, OpOutput))

  signature <- OpSignature(
    input_types = ValueTypes(input_types),
    output_types = output_types
  )

  inputs <- OpInputs(
    values = OpInputValues(input_value_ids),
    custom_attrs = list(callee = callee$id)
  )

  op <- OpCall(
    inputs = inputs,
    outputs = outputs,
    signature = signature
  )

  func$body <- FuncBody(c(func$body, list(op)))

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

#' @export
repr.OpCall <- function(x, toplevel = TRUE, ...) {
  paste0(
    repr(x$outputs),
    " = func.call ",
    repr(x$inputs$custom_attrs$callee),
    "(",
    repr(x$inputs$values),
    ") : ",
    repr(x$signature)
  )
}
