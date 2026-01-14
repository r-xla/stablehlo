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
    cli_abort(
      "size(start_indices) must equal rank(operand)"
    )
  }

  if (length(slice_sizes_data) != operand_rank) {
    cli_abort(
      "size(slice_sizes) must equal rank(operand)"
    )
  }

  # (C3)
  if (length(start_indices) > 0) {
    start_types <- lapply(start_indices, function(x) x$type)
    if (length(unique(start_types)) != 1) {
      cli_abort("All start_indices must have the same type")
    }

    # Check that all start_indices are 0-dimensional tensors
    for (i in seq_along(start_indices)) {
      assert_vt_is_tensor(start_indices[[i]])
      if (length(shape(start_indices[[i]])) != 0) {
        cli_abort("All start_indices must be 0-dimensional tensors")
      }
    }
  }

  # (C4)
  if (any(slice_sizes_data > shape(operand))) {
    cli_abort("slice_sizes must not be greater than operand's shape")
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
