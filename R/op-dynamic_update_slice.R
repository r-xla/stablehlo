#' @include op.R hlo.R
NULL

OpDynamicUpdateSlice <- new_Op("OpDynamicUpdateSlice", "dynamic_update_slice")

# fmt: skip
#' @rdname hlo_dynamic_update_slice
#' @export
infer_types_dynamic_update_slice <- function( # nolint
  operand,
  update,
  ...
) {
  assert_vt_is_tensor(operand)
  assert_vt_is_tensor(update)
  assert_vts_are_tensors(...)
  start_indices <- list(...)
  for (vt in start_indices) {
    if (length(shape(vt)) != 0) {
      cli_abort("All start_indices must be 0-dimensional tensors")
    }
  }

  operand_rank <- length(shape(operand))

  # (C2)
  assert_vts_have_same_dtype(operand, update)

  # (C3)
  if (length(shape(update)) != operand_rank) {
    cli_abort("rank(update) must equal rank(operand)")
  }

  # (C4)
  if (length(start_indices) != operand_rank) {
    cli_abort("size(start_indices) must equal rank(operand)")
  }

  # (C6)
  if (any(shape(update) > shape(operand))) {
    cli_abort("shape(update) must not be greater than shape(operand)")
  }

  # (C1)
  ValueTypes(list(
    ValueType(
      TensorType(
        dtype = operand$type$dtype,
        shape = Shape(shape(operand))
      )
    )
  ))
}

hlo_dynamic_update_slice_impl <- hlo_fn(
  OpDynamicUpdateSlice,
  infer_types_dynamic_update_slice
)

#' @templateVar mnemonic dynamic_update_slice
#' @template op
#' @export
hlo_dynamic_update_slice <- function(
  operand,
  update,
  ...
) {
  start_indices <- list(...)

  hlo_dynamic_update_slice_impl(
    values = c(list(operand = operand, update = update), start_indices)
  )
}
