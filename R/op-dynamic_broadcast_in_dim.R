#' @include op.R hlo.R
NULL

OpDynamicBroadcastInDim <- new_Op(
  "OpDynamicBroadcastInDim",
  "dynamic_broadcast_in_dim"
)

#' @rdname hlo_dynamic_broadcast_in_dim
#' @param output_dimensions ([`FuncValue`] | [`ValueType`])\cr
#'   A rank-1 integer tensor holding the result's axis sizes, one element per
#'   axis of the result. This is what makes the op dynamic: the sizes are
#'   values in the program rather than part of its type.
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
    if (is.na(op_dim) || is.na(out_dim)) {
      next
    }
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
#' @template op
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
