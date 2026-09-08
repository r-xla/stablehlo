#' @include op.R hlo.R
NULL

OpDynamicReshape <- new_Op("OpDynamicReshape", "dynamic_reshape")

#' @rdname hlo_dynamic_reshape
#' @param output_shape ([`FuncValue`] | [`ValueType`])\cr
#'   A rank-1 integer tensor holding the result's axis sizes.
#' @param shape (`integer()`)\cr
#'   The result's static shape, with `NA` at each axis whose size is only known
#'   at run time.
#' @export
infer_types_dynamic_reshape <- function(operand, output_shape, shape) {
  assert_vt_is_tensor(operand)
  assert_vt_is_tensor(output_shape)
  assert_shapevec_dyn(shape)
  shape <- as.integer(shape)

  declared <- shape(output_shape)
  if (length(declared) != 1L) {
    cli_abort(c(
      "{.arg output_shape} must be a rank-1 tensor.",
      x = "Got shape {shapevec_repr(declared)}."
    ))
  }

  # (C2) `size(operand) = size(result)`. Both counts are unknown as soon as
  # either side has a dynamic axis, and then this is a run-time question --
  # which is the whole reason to reach for this op over `reshape`.
  if (must_nelts_ne(shape(operand), shape)) {
    cli_abort(c(
      "{.arg operand} and the result must have the same number of elements.",
      x = "Got {shapevec_repr(shape(operand))} and {shapevec_repr(shape)}."
    ))
  }

  # (C4) `size(output_shape) = rank(result)`.
  if (must_ne(declared, length(shape))) {
    cli_abort(c(
      "{.arg output_shape} must have one element per axis of the result.",
      x = "Got {vec_repr(declared)} elements for a rank-{length(shape)} result."
    ))
  }

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
#' @template op
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
