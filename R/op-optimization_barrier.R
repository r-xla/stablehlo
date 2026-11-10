#' @include op.R hlo.R
NULL

OpOptimizationBarrier <- new_Op("OpOptimizationBarrier", "optimization_barrier")

#' @rdname hlo_optimization_barrier
#' @export
infer_types_optimization_barrier <- function(...) {
  # (I1) `operand` is a variadic number of tensors or tokens, which every
  # `ValueType` is.
  # (C1) `type(operand...) = type(result...)`.
  ValueTypes(list(...))
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
  func = NULL,
  output_types = NULL
) {
  hlo_optimization_barrier_impl(
    values = list(...),
    simplify = simplify,
    output_types = output_types,
    func = func
  )
}
