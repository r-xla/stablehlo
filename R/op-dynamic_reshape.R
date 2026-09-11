#' @include op.R hlo.R
NULL

OpDynamicReshape <- new_Op("OpDynamicReshape", "dynamic_reshape")

#' @rdname hlo_dynamic_reshape
#' @param shape (`integer()`)\cr
#'   The result's static shape, with `NA` at each axis whose size is only known
#'   at run time.
#' @export
infer_types_dynamic_reshape <- function(operand, output_shape, shape) {
  assert_vt_is_tensor(operand)
  assert_vt_is_tensor(output_shape)
  assert_shapevec_dyn(shape)
  shape <- as.integer(shape)

  # (C2) `size(operand) = size(result)`. Both counts are unknown as soon as
  # either side has a dynamic axis, and then this is a run-time question --
  # which is the whole reason to reach for this op over `reshape`.
  if (provably_nelts_ne(shape(operand), shape)) {
    cli_abort(c(
      "{.arg operand} and the result must have the same number of elements.",
      x = "Got {shapevec_repr(shape(operand))} and {shapevec_repr(shape)}."
    ))
  }

  # (C4) `size(output_shape) = rank(result)`, plus I2's type.
  assert_size_operand(output_shape, length(shape))

  # (C1) The element type is the operand's.
  ValueTypes(list(
    ValueType(TensorType(dtype = operand$type$dtype, shape = Shape(shape)))
  ))
}

hlo_dynamic_reshape_impl <- hlo_fn(
  OpDynamicReshape,
  infer_types_dynamic_reshape
)

#' @templateVar mnemonic dynamic_reshape
#' @templateVar not_func_variables shape
#' @template op
#' @details
#' Note that `shape` is a *claim*, not a check: nothing here can verify it,
#' since the sizes it describes are data. Where StableHLO can constant-fold the
#' size operands it will verify the claim itself and reject a wrong one
#' downstream.
#' @export
hlo_dynamic_reshape <- function(
  operand,
  output_shape,
  shape,
  output_types = NULL
) {
  hlo_dynamic_reshape_impl(
    values = list(operand = operand, output_shape = output_shape),
    output_types = output_types,
    custom_attrs = list(shape = as.integer(shape))
  )
}
