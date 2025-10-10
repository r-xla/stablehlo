#' @include op.R hlo.R
NULL

# Technicall this is not listed as an Op, but a Func's body is defined as {Op}, so I guess it kind of is?
OpReturn <- new_class(
  "Return",
  parent = Op,
  constructor = function(inputs, outputs = OpOutputs(), signature = NULL) {
    if (length(outputs@items)) {
      cli_abort("OpReturn op must not have outputs.")
    }
    if (length(signature@output_types@items)) {
      cli_abort("Invalid signature for ReturnOp.")
    }

    new_object(Op(
      name = OpName(OpMnemonic("return")),
      inputs = inputs,
      outputs = outputs,
      signature = signature
    ))
  }
)

hlo_return_impl <- hlo_fn(OpReturn, infer_types_return, TRUE)

#' @title Create a return operation
#' @param ... ([`FuncVariable`])\cr
#' @export
hlo_return <- function(...) {
  dots <- list(...)
  restore_previous_func()
  hlo_return_impl(values = dots)
}

infer_types_return <- function(...) {
  dots <- list(...)
  lapply(dots, function(x) {
    stopifnot(inherits(x, ValueType))
  })
  ValueTypes()
}

method(repr, OpReturn) <- function(x, toplevel = TRUE) {
  paste0(
    repr(x@outputs),
    if (toplevel) "\"func.return\"" else "\"stablehlo.return\"",
    repr(x@inputs),
    ": ",
    repr(x@signature)
  )
}
