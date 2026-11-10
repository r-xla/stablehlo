#' @include op.R hlo.R
NULL

OpOptimizationBarrier <- new_Op(
  "OpOptimizationBarrier",
  "optimization_barrier",
  # The short form names one type per operand, not one type for all of them.
  same_type_form = FALSE
)

#' @rdname hlo_optimization_barrier
#' @export
infer_types_optimization_barrier <- function(...) {
  operands <- list(...)
  # The spec allows no operands, but an op without results has no line that
  # MLIR parses, and such a barrier does nothing.
  if (length(operands) == 0L) {
    cli_abort("There must be at least one operand.")
  }
  # (I1) `operand` is a variadic number of tensors or tokens, which every
  # `ValueType` is.
  # (C1) `type(operand...) = type(result...)`.
  ValueTypes(operands)
}

hlo_optimization_barrier_impl <- hlo_fn(
  OpOptimizationBarrier,
  infer_types_optimization_barrier
)

#' @templateVar mnemonic optimization_barrier
#' @templateVar not_func_variables simplify
#' @template op
#' @template param_simplify
#' @export
hlo_optimization_barrier <- function(
  ...,
  simplify = TRUE,
  output_types = NULL
) {
  hlo_optimization_barrier_impl(
    values = list(...),
    simplify = simplify,
    output_types = output_types
  )
}
