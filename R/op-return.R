#' @include op.R hlo.R
NULL

# Technically this is not listed as an Op, but a Func's body is defined as {Op}, so I guess it kind of is?
OpReturn <- function(inputs, outputs = OpOutputs(), signature = NULL) {
  if (length(outputs$items)) {
    cli_abort("OpReturn op must not have outputs.")
  }
  if (length(signature$output_types$items)) {
    cli_abort("Invalid signature for ReturnOp.")
  }

  base_op <- Op(
    name = OpName(OpMnemonic("return")),
    inputs = inputs,
    outputs = outputs,
    signature = signature
  )
  class(base_op) <- c("OpReturn", "Op")
  base_op
}

hlo_return_impl <- hlo_fn(OpReturn, infer_types_return, TRUE)

#' @title Return Values
#' @description
#' Specifies the return values of a [`Func`] and finalize it.
#' @param ... ([`FuncValue`])\cr
#'   Return values. There must be at least one.
#' @template param_func
#' @return ([`Func`])
#' @export
hlo_return <- function(..., func = .current_func()) {
  dots <- list(...)
  if (!length(dots)) {
    stop("hlo_return must have at least one argument")
  }
  output_count <- length(dots)
  alias_indices <- vapply(
    func$inputs$items[vapply(func$inputs$items, is.null, logical(1))],
    \(x) as.integer(x$alias),
    integer(1)
  )
  if (any(alias_indices < 0L | alias_indices >= output_count)) {
    cli_abort("Aliased output index out of bounds in inputs")
  }
  if (anyDuplicated(alias_indices)) {
    cli_abort("Multiple inputs alias to the same output index")
  }
  func <- hlo_return_impl(values = dots)
  maybe_restore_previous_func()
  return(func)
}

#' @rdname hlo_return
#' @export
infer_types_return <- function(...) {
  assert_vts_are_tensors(...)
  ValueTypes()
}

#' @export
repr.OpReturn <- function(x, toplevel = TRUE, ...) {
  paste0(
    repr(x$outputs),
    if (toplevel) "\"func.return\"" else "\"stablehlo.return\"",
    repr(x$inputs),
    ": ",
    repr(x$signature)
  )
}
