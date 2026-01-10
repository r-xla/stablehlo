#' @include op.R hlo.R
NULL

OpSlice <- new_Op("OpSlice", "slice")

#' @rdname hlo_slice
#' @export
infer_types_slice <- function(
  operand,
  start_indices,
  limit_indices,
  strides
) {
  assert_vt_is_tensor(operand)

  # Extract indices and operand rank
  operand_rank <- length(shape(operand))
  start_idx <- start_indices$data
  limit_idx <- limit_indices$data
  stride_vals <- strides$data

  # (C2) size(start_indices) = size(limit_indices) = size(strides) = rank(operand)
  if (length(start_idx) != length(limit_idx)) {
    cli_abort("start_indices must have same length as limit indices")
  }

  if (length(start_idx) != length(stride_vals)) {
    cli_abort(
      "strides must have same length as start_indices and limit_indices"
    )
  }

  if (length(start_idx) != operand_rank) {
    cli_abort(
      "length of start_indices, limit_indices and strides must be equal to operand's rank"
    )
  }

  # (C3) 0 <= start_indices <= limit_indices <= shape(operand)
  operand_shape <- shape(operand)

  if (any(start_idx < 0)) {
    invalid_positions <- which(start_idx < 0)
    error_slice_index(
      arg = "start_indices",
      index = start_idx[invalid_positions],
      lower = 0L,
      upper = operand_shape[invalid_positions]
    )
  }

  if (any(start_idx > limit_idx)) {
    invalid_positions <- which(start_idx > limit_idx)
    error_slice_index(
      arg = "start_indices",
      index = start_idx[invalid_positions],
      lower = 0L,
      upper = limit_idx[invalid_positions]
    )
  }

  if (any(limit_idx > operand_shape)) {
    invalid_positions <- which(limit_idx > operand_shape)
    error_slice_index(
      arg = "limit_indices",
      index = limit_idx[invalid_positions],
      lower = start_idx[invalid_positions],
      upper = operand_shape[invalid_positions]
    )
  }

  # (C4) 0 < strides.
  if (any(stride_vals < 0)) {
    cli_abort("strides must be non-negative")
  }

  # (C5) shape(result) = ceil((limit_indices - start_indices) / strides)
  result_dims <- ceiling((limit_idx - start_idx) / stride_vals)

  ValueTypes(list(
    ValueType(
      TensorType(
        dtype = operand$type$dtype,
        shape = Shape(result_dims)
      )
    )
  ))
}

hlo_slice_impl <- hlo_fn(
  OpSlice,
  infer_types_slice
)

#' @templateVar mnemonic slice
#' @template op
#' @export
hlo_slice <- function(
  operand,
  start_indices,
  limit_indices,
  strides
) {
  hlo_slice_impl(
    values = list(operand = operand),
    attrs = list(
      constant_attr(
        "start_indices",
        as.integer(start_indices),
        dtype = "i64",
        shape = c()
      ),
      constant_attr(
        "limit_indices",
        as.integer(limit_indices),
        dtype = "i64",
        shape = c()
      ),
      constant_attr("strides", as.integer(strides), dtype = "i64", shape = c())
    )
  )
}
