#' @include op.R hlo.R
NULL

OpBroadcastInDim <- new_Op("OpBroadcastInDim", "broadcast_in_dim")

#' @rdname hlo_broadcast_in_dim
#' @export
infer_types_broadcast_in_dim <- function(
  operand,
  broadcast_dimensions,
  shape
) {
  assert_vt_is_tensor(operand)
  assert_const(broadcast_dimensions, dtype = "i64", ndims = 1L)
  assert_shapevec(shape)

  operand_dims <- shape(operand)
  result_dims <- as.integer(shape)

  bdims <- broadcast_dimensions$data

  # (C2)
  if (length(bdims) != length(operand_dims)) {
    cli_abort(c(
      "Length of {.arg broadcast_dimensions} must equal rank of {.arg operand}.",
      x = "Got {length(bdims)} broadcast_dimensions for operand of rank {length(operand_dims)}."
    ))
  }

  # (C3)
  if (any(bdims < 0L | bdims >= length(result_dims))) {
    error_index_out_of_bounds(
      arg = "broadcast_dimensions",
      index = bdims,
      lower = 0L,
      upper = length(result_dims)
    )
  }

  # (C4)
  if (anyDuplicated(bdims)) {
    error_dimension_uniqueness(
      arg = "broadcast_dimensions",
      dimensions = bdims
    )
  }

  # (C5) For all d in axes(operand):
  #   shape(operand, d) = 1 OR shape(operand, d) = shape(result, broadcast_dimensions[d])
  for (d in seq_along(bdims)) {
    op_dim <- operand_dims[d]
    out_dim <- result_dims[bdims[d] + 1L]
    if ((op_dim != out_dim) && op_dim != 1L) {
      error_dim_size_mismatch(
        arg1 = "operand",
        arg2 = "result",
        dim1 = d - 1L,
        dim2 = bdims[d],
        shape1 = operand_dims,
        shape2 = result_dims
      )
    }
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

hlo_broadcast_in_dim_impl <- hlo_fn(
  OpBroadcastInDim,
  infer_types_broadcast_in_dim
)

#' @templateVar mnemonic broadcast_in_dim
#' @template op
#' @export
hlo_broadcast_in_dim <- function(
  operand,
  broadcast_dimensions,
  shape
) {
  hlo_broadcast_in_dim_impl(
    values = list(operand = operand),
    attrs = list(
      constant_attr(
        "broadcast_dimensions",
        as.integer(broadcast_dimensions),
        dtype = "i64",
        shape = length(broadcast_dimensions)
      )
    ),
    custom_attrs = list(shape = as.integer(shape))
  )
}
