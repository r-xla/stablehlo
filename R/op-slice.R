#' @include op.R hlo.R
NULL

OpSlice <- new_Op("OpSlice", "slice")

infer_types_slice <- function(
  operand,
  start_indices,
  limit_indices,
  strides
) {
  stopifnot(inherits(operand@type, TensorType))
  # stopifnot(inherits(start_indices@type, TensorType))
  # stopifnot(inherits(limit_indices@type, TensorType))
  # stopifnot(inherits(strides@type, TensorType))
  # assert_one_of(start_indices@type@dtype, IntegerType)
  # assert_one_of(limit_indices@type@dtype, IntegerType)
  # assert_one_of(strides@type@dtype, IntegerType)

  # Extract indices and operand rank
  operand_rank <- length(shape(operand))
  start_idx <- start_indices@value@data
  limit_idx <- limit_indices@value@data
  stride_vals <- strides@value@data

  # (C2) size(start_indices) = size(limit_indices) = size(strides) = rank(operand)
  if (length(start_idx) != length(limit_idx)) {
    cli::cli_abort("start_indices must have same length as limit indices")
  }

  if (length(start_idx) != length(stride_vals)) {
    cli::cli_abort(
      "strides must have same length as start_indices and limit_indices"
    )
  }

  if (length(start_idx) != operand_rank) {
    cli::cli_abort(
      "length of start_indices, limit_indices and strides must be equal to operand's rank"
    )
  }

  # (C3) 0 <= start_indices <= limit_indices <= shape(operand)
  if (any(start_idx < 0)) {
    cli::cli_abort("start_indices must be non_negative")
  }

  if (any(start_idx > limit_idx)) {
    cli::cli_abort("start_indices must not be greater than limit_indices")
  }

  if (any(limit_idx > shape(operand))) {
    cli::cli_abort("limit_indices must not be greater than operand's shape")
  }

  # (C4) 0 < strides.
  if (any(stride_vals < 0)) {
    cli::cli_abort("strides must be non-negative")
  }

  # (C5) shape(result) = ceil((limit_indices - start_indices) / strides)
  result_dims <- ceiling((limit_idx - start_idx) / stride_vals)

  ValueTypes(list(
    ValueType(
      TensorType(
        dtype = operand@type@dtype,
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
  start_attr <- hlo_tensor(
    as.integer(start_indices),
    dtype = "i64",
    func = Func()
  )
  limit_attr <- hlo_tensor(
    as.integer(limit_indices),
    dtype = "i64",
    func = Func()
  )
  stride_attr <- hlo_tensor(as.integer(strides), dtype = "i64", func = Func())

  hlo_slice_impl(
    values = list(operand = operand),
    attrs = list(
      start_indices = start_attr,
      limit_indices = limit_attr,
      strides = stride_attr
    )
  )
}

method(repr, OpSlice) <- function(x) {
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
