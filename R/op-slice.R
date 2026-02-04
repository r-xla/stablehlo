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
  assert_const(start_indices, dtype = IntegerType(64L), ndims = 1L)
  assert_const(limit_indices, dtype = IntegerType(64L), ndims = 1L)
  assert_const(strides, dtype = IntegerType(64L), ndims = 1L)

  operand_rank <- length(shape(operand))
  start_idx <- start_indices$data
  limit_idx <- limit_indices$data
  stride_vals <- strides$data

  # (C2)
  if (length(start_idx) != length(limit_idx)) {
    cli_abort(c(
      "{.arg start_indices} must have same length as {.arg limit_indices}",
      x = "Got lengths {length(start_idx)} and {length(limit_idx)}."
    ))
  }

  if (length(start_idx) != length(stride_vals)) {
    cli_abort(c(
      "{.arg strides} must have same length as {.arg start_indices} and {.arg limit_indices}",
      x = "Got lengths {length(start_idx)}, {length(limit_idx)} and {length(stride_vals)}."
    ))
  }

  if (length(start_idx) != operand_rank) {
    cli_abort(c(
      "length of {.arg start_indices}, {.arg limit_indices} and {.arg strides} must be equal to operand's rank ({operand_rank}).",
      x = "Got length {length(start_idx)}."
    ))
  }

  # (C3)
  operand_shape <- shape(operand)

  if (any(start_idx < 0)) {
    invalid_positions <- which(start_idx < 0)
    error_index_out_of_bounds(
      arg = "start_indices",
      index = start_idx[invalid_positions],
      lower = 0L,
      upper = operand_shape[invalid_positions]
    )
  }

  if (any(start_idx > limit_idx)) {
    invalid_positions <- which(start_idx > limit_idx)
    error_index_out_of_bounds(
      arg = "start_indices",
      index = start_idx[invalid_positions],
      lower = 0L,
      upper = limit_idx[invalid_positions]
    )
  }

  if (any(limit_idx > operand_shape)) {
    invalid_positions <- which(limit_idx > operand_shape)
    error_index_out_of_bounds(
      arg = "limit_indices",
      index = limit_idx[invalid_positions],
      lower = start_idx[invalid_positions],
      upper = operand_shape[invalid_positions]
    )
  }

  # (C4)
  if (any(stride_vals < 0)) {
    cli_abort("{.arg strides} must be non-negative")
  }

  # (C5)
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
  start_int <- as.integer(start_indices)
  limit_int <- as.integer(limit_indices)
  strides_int <- as.integer(strides)

  hlo_slice_impl(
    values = list(operand = operand),
    attrs = list(
      constant_attr(
        "start_indices",
        start_int,
        dtype = "i64",
        shape = length(start_int)
      ),
      constant_attr(
        "limit_indices",
        limit_int,
        dtype = "i64",
        shape = length(limit_int)
      ),
      constant_attr(
        "strides",
        strides_int,
        dtype = "i64",
        shape = length(strides_int)
      )
    )
  )
}
