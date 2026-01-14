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

  result_dims <- as.integer(shape)

  # (C2)
  if (prod(shape(operand)) != prod(result_dims)) {
    cli_abort(
      "Size of output must equal to size of operand",
      # fmt: skip
      x = "Got shape(operand) ={.val {shapevec_repr(shape(operand))}} and shape(result) ={.val {shapevec_repr(result_dims)}}." # nolint
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
  shape
) {
  hlo_reshape_impl(
    values = list(operand = operand),
    custom_attrs = list(shape = as.integer(shape))
  )
}
