#' @include op.R hlo.R
NULL

# Technicall this is not listed as an Op, but a Func's body is defined as {Op}, so I guess it kind of is?
Return <- new_class(
  "Return",
  parent = Op,
  constructor = function(inputs, outputs = OpOutputs(), signature = NULL) {
    if (length(outputs@items)) {
      stop("Return op must not have outputs.")
    }
    if (length(signature@output_types@items)) {
      stop("Invalid signature for Return op.")
    }
    new_object(
      Op,
      name = OpName(OpMnemonic("return")),
      inputs = inputs,
      outputs = outputs,
      signature = signature
    )
  }
)

hlo_return_impl <- hlo_fn(Return, infer_types_return, TRUE)

#' @title Create a return operation
#' @param ... ([`FuncPointer`])\cr
#' @export
hlo_return <- function(...) {
  dots <- list(...)
  hlo_return_impl(values = dots)
}

infer_types_return <- function(...) {
  dots <- list(...)
  lapply(dots, function(x) {
    stopifnot(inherits(x, ValueType))
  })
  ValueTypes()
}
