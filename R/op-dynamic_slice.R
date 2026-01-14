#' @include op.R hlo.R
NULL

OpDynamicSlice <- new_Op("OpDynamicSlice", "dynamic_slice")

#' @rdname hlo_dynamic_slice
#' @export
infer_types_dynamic_slice <- function(
  operand,
  ...,
  slice_sizes
) {
  assert_vt_is_tensor(operand)

  start_indices <- list(...)
  operand_rank <- length(shape(operand))
  slice_sizes_data <- slice_sizes$data

  # (C2)
  if (length(start_indices) != operand_rank) {
    cli_abort(c(
      "size(start_indices) must equal rank(operand).",
      i = "Got {length(start_indices)} start_indices and rank {operand_rank}."
    ))
  }

  if (length(slice_sizes_data) != operand_rank) {
    cli_abort(c(
      "size(slice_sizes) must equal rank(operand).",
      i = "Got {length(slice_sizes_data)} slice_sizes and rank {operand_rank}."
    ))
  }

  # (C3)
  if (length(start_indices) > 0) {
    start_types <- lapply(start_indices, function(x) x$type)
    if (length(unique(start_types)) != 1) {
      # fmt: skip
      type_strs <- vapply(start_types, repr, character(1)) # nolint
      cli_abort(c(
        "All start_indices must have the same type.",
        i = "Got types: {paste0(type_strs, collapse = ', ')}."
      ))
    }

    # Check that all start_indices are 0-dimensional tensors
    for (i in seq_along(start_indices)) {
      assert_vt_is_tensor(start_indices[[i]])
      if (length(shape(start_indices[[i]])) != 0) {
        error_unexpected_type(
          arg = "start_indices",
          index = i - 1L, # 0-based
          expected = "must be 0-dimensional tensors",
          actual = paste("shape", shapevec_repr(shape(start_indices[[i]])))
        )
      }
    }
  }

  # (C4)
  if (any(slice_sizes_data > shape(operand))) {
    cli_abort(c(
      "slice_sizes must not be greater than operand's shape.",
      i = "Got slice_sizes {shapevec_repr(slice_sizes_data)} and operand shape {shapevec_repr(shape(operand))}."
    ))
  }

  # (C1), (C5)
  ValueTypes(list(
    ValueType(
      TensorType(
        dtype = operand$type$dtype,
        shape = Shape(slice_sizes_data)
      )
    )
  ))
}

hlo_dynamic_slice_impl <- hlo_fn(
  OpDynamicSlice,
  infer_types_dynamic_slice
)

#' @templateVar mnemonic dynamic_slice
#' @template op
#' @export
hlo_dynamic_slice <- function(
  operand,
  ...,
  slice_sizes
) {
  start_indices <- list(...)

  hlo_dynamic_slice_impl(
    values = c(list(operand = operand), start_indices),
    attrs = list(
      constant_attr(
        "slice_sizes",
        as.integer(slice_sizes),
        dtype = "i64",
        shape = c()
      )
    )
  )
}
