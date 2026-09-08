#' @include op.R hlo.R
NULL

OpReshape <- new_Op("OpReshape", "reshape")

#' @rdname hlo_reshape
#' @export
infer_types_reshape <- function(
  operand,
  shape
) {
  assert_vt_is_tensor(operand)

  # The result shape is an attribute, so it must be static: StableHLO
  # requires `reshape`'s result to be statically shaped. Use
  # `hlo_dynamic_reshape()` for a result shape the program computes.
  assert_shapevec(shape)
  result_dims <- as.integer(shape)

  # (C2) Element counts must agree -- but only when both are known. A dynamic
  # axis on either side makes its count `NA` (prod() propagates), and then
  # whether the counts match is a run-time question: reshaping `tensor<?xf32>`
  # to `tensor<3xf32>` is legal exactly when the operand turns out to hold 3
  # elements, which is not knowable here.
  if (must_nelts_ne(shape(operand), result_dims)) {
    cli_abort(
      "Size of output must equal to size of {.arg operand}",
      # fmt: skip
      x = "Got shape(operand) = {shapevec_repr(shape(operand))} and shape(result) = {shapevec_repr(result_dims)}." # nolint
    )
  }

  # (C1)
  ValueTypes(list(
    ValueType(
      TensorType(
        dtype = operand$type$dtype,
        shape = Shape(result_dims)
      )
    )
  ))
}

hlo_reshape_impl <- hlo_fn(
  OpReshape,
  infer_types_reshape
)

#' @templateVar mnemonic reshape
#' @template op
#' @export
hlo_reshape <- function(
  operand,
  shape,
  output_types = NULL
) {
  hlo_reshape_impl(
    values = list(operand = operand),
    output_types = output_types,
    custom_attrs = list(shape = as.integer(shape))
  )
}
