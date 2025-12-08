#' @include op.R hlo.R
NULL

OpReshape <- new_Op("OpReshape", "reshape")

#' @rdname hlo_reshape
#' @export
infer_types_reshape <- function(
  operand,
  shape_out
) {
  assert_vt_is_tensor(operand)

  result_dims <- as.integer(shape_out)

  # (C2) size(operand) = size(result)
  if (prod(shape(operand)) != prod(result_dims)) {
    cli_abort(
      "Size of output must equal to size of operand",
      x = "Got {.val {prod(shape(operand))}} and {.val {prod(result_dims)}}."
    )
  }

  ValueTypes(list(
    ValueType(
      TensorType(
        dtype = operand@type@dtype,
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
  shape_out
) {
  hlo_reshape_impl(
    values = list(operand = operand),
    custom_attrs = list(shape_out = as.integer(shape_out))
  )
}

method(repr, OpReshape) <- function(x, ...) {
  paste0(
    repr(x@outputs),
    " = ",
    repr(x@name),
    " ",
    repr(x@inputs, simplify_dense = TRUE),
    ": ",
    repr(x@signature)
  )
}
