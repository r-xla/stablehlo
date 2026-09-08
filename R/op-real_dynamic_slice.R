#' @include op.R hlo.R
NULL

OpRealDynamicSlice <- new_Op("OpRealDynamicSlice", "real_dynamic_slice")

#' @rdname hlo_real_dynamic_slice
#' @param start_indices,limit_indices,strides ([`FuncValue`] | [`ValueType`])\cr
#'   Rank-1 integer tensors, one element per axis of `operand`.
#' @param shape (`integer()`)\cr
#'   The result's static shape, with `NA` at each axis whose size is only known
#'   at run time.
#' @export
infer_types_real_dynamic_slice <- function(
  operand,
  start_indices,
  limit_indices,
  strides,
  shape
) {
  assert_vts_are_tensors(
    operand = operand,
    start_indices = start_indices,
    limit_indices = limit_indices,
    strides = strides
  )
  assert_shapevec_dyn(shape)
  shape <- as.integer(shape)

  rank <- length(shape(operand))
  index_args <- list(
    start_indices = start_indices,
    limit_indices = limit_indices,
    strides = strides
  )
  for (nm in names(index_args)) {
    declared <- shape(index_args[[nm]])
    if (length(declared) != 1L) {
      cli_abort(c(
        "{.arg {nm}} must be a rank-1 tensor.",
        x = "Got shape {shapevec_repr(declared)}."
      ))
    }
    if (must_ne(declared, rank)) {
      cli_abort(c(
        "{.arg {nm}} must have one element per axis of {.arg operand}.",
        x = "Got {vec_repr(declared)} elements for a rank-{rank} operand."
      ))
    }
  }

  # The extents come from the index tensors, which are data, so the result
  # shape is the hint. Its rank is still the operand's.
  if (length(shape) != rank) {
    cli_abort(c(
      "The result must have the same rank as {.arg operand}.",
      x = "Got {shapevec_repr(shape)} for a rank-{rank} operand."
    ))
  }

  ValueTypes(list(
    ValueType(TensorType(dtype = operand$type$dtype, shape = Shape(shape)))
  ))
}

hlo_real_dynamic_slice_impl <- hlo_fn(
  OpRealDynamicSlice,
  infer_types_real_dynamic_slice
)

#' @title Real Dynamic Slice Operator
#' @description
#' Slices `operand` with start, limit and stride vectors supplied as *values*
#' rather than attributes, so the result's extents may be computed by the
#' program -- from its input data, even. That is what distinguishes it from
#' [`hlo_slice()`] (all three static) and [`hlo_dynamic_slice()`] (dynamic
#' start, but static sizes, hence a static result shape).
#'
#' This op is in the StableHLO dialect but **not** in SPEC.md, which documents
#' no op with a data-dependent result extent. It is here because it is the only
#' way to express one: a program that computes how much of a buffer is live --
#' the count of distinct elements, of rows passing a filter -- and returns just
#' that much.
#'
#' Note that XLA cannot compile it (`can't be translated to XLA HLO`), and
#' shape refinement cannot remove it either, since no shape in the program
#' determines the extent. It needs a backend that compiles dynamic shapes.
#' @param operand ([`FuncValue`])\cr
#'   The array to slice.
#' @param output_types (`list()` of [`ValueType`] | `NULL`)\cr
#'   Output types known ahead of time (e.g. from type inference at trace
#'   time). When provided, type inference and its input validation are
#'   skipped.
#' @return [`FuncValue`]
#' @export
hlo_real_dynamic_slice <- function(
  operand,
  start_indices,
  limit_indices,
  strides,
  shape,
  output_types = NULL
) {
  hlo_real_dynamic_slice_impl(
    values = list(
      operand = operand,
      start_indices = start_indices,
      limit_indices = limit_indices,
      strides = strides
    ),
    output_types = output_types,
    custom_attrs = list(shape = as.integer(shape))
  )
}
