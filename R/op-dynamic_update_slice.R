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
  for (i in seq_along(start_indices)) {
    vt <- start_indices[[i]]
    if (length(shape(vt)) != 0) {
      error_unexpected_list_type(
        arg = "start_indices",
        index = i - 1L, # 0-based
        expected = "must be 0-dimensional tensors",
        actual = paste("shape", shapevec_repr(shape(vt)))
      )
    }
  }

  operand_rank <- length(shape(operand))

  # (C2)
  assert_vts_have_same_dtype(operand, update)

  # (C3)
  if (length(shape(update)) != operand_rank) {
    cli_abort(c(
      "rank(update) must equal rank(operand).",
      x = "Got rank(update) = {length(shape(update))} and rank(operand) = {operand_rank}."
    ))
  }

  # (C4)
  if (length(start_indices) != operand_rank) {
    cli_abort(c(
      "length(start_indices) must equal rank(operand).",
      x = "Got {length(start_indices)} start_indices and rank {operand_rank}."
    ))
  }

  # (C6)
  if (any(shape(update) > shape(operand))) {
    cli_abort(c(
      "shape(update) must not be greater than shape(operand).",
      x = "Got shape(update) {shapevec_repr(shape(update))} and shape(operand) {shapevec_repr(shape(operand))}."
    ))
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
