#' @include op.R hlo.R
NULL

OpDynamicBroadcastInDim <- new_Op(
  "OpDynamicBroadcastInDim",
  "dynamic_broadcast_in_dim"
)

#' @rdname hlo_dynamic_broadcast_in_dim
#' @param broadcast_dimensions (`integer()`)\cr
#'   Maps each axis of `operand` to an axis of the result.
#' @param shape (`integer()`)\cr
#'   The result's static shape, with `NA` at each axis whose size is only known
#'   at run time. It cannot be inferred from the operands, because
#'   `output_dimensions` is data.
#' @export
infer_types_dynamic_broadcast_in_dim <- function(
  operand,
  output_dimensions,
  broadcast_dimensions,
  shape
) {
  assert_vt_is_tensor(operand)
  assert_vt_is_tensor(output_dimensions)
  assert_const(broadcast_dimensions, dtype = "i64", naxes = 1L)
  # Unlike the static op, `NA` is allowed here: it is what a dynamic axis is
  # spelled as, and the whole point of this op.
  assert_shapevec_dyn(shape)

  # (C7) `size(output_dimensions) = rank(result)`, plus I2's type.
  assert_size_operand(output_dimensions, length(shape))

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
    error_dimension_uniqueness(arg = "broadcast_dimensions", dimensions = bdims)
  }

  # (C5) as for the static op, but only where both sizes are known: a dynamic
  # axis on either side is checked at run time, not here.
  for (d in seq_along(bdims)) {
    op_dim <- operand_dims[d]
    out_dim <- result_dims[bdims[d] + 1L]
    if (provably_ne(op_dim, out_dim) && provably_ne(op_dim, 1L)) {
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
    ValueType(TensorType(
      dtype = operand$type$dtype,
      shape = Shape(result_dims)
    ))
  ))
}

hlo_dynamic_broadcast_in_dim_impl <- hlo_fn(
  OpDynamicBroadcastInDim,
  infer_types_dynamic_broadcast_in_dim
)

#' @templateVar mnemonic dynamic_broadcast_in_dim
#' @templateVar not_func_variables broadcast_dimensions,shape
#' @template op
#' @details
#' Note that `shape` is a *claim*, not a check: nothing here can verify it,
#' since the sizes it describes are data. Where StableHLO can constant-fold the
#' size operands it will verify the claim itself and reject a wrong one
#' downstream.
#' @export
hlo_dynamic_broadcast_in_dim <- function(
  operand,
  output_dimensions,
  broadcast_dimensions,
  shape,
  output_types = NULL
) {
  hlo_dynamic_broadcast_in_dim_impl(
    values = list(operand = operand, output_dimensions = output_dimensions),
    output_types = output_types,
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
