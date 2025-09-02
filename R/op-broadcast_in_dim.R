#' @include op.R hlo.R
NULL

BroadcastInDim <- new_Op("BroadcastInDim", "broadcast_in_dim")

infer_types_broadcast_in_dim <- function(
  operand,
  broadcast_dimensions,
  shape_out
) {
  stopifnot(inherits(operand@type, TensorType))

  # Extract operand dims and target result dims
  operand_dims <- shape(operand)
  result_dims <- as.integer(shape_out)

  bdims <- broadcast_dimensions@value@data

  # (C2) size(broadcast_dimensions) = rank(operand)
  if (length(bdims) != length(operand_dims)) {
    cli::cli_abort("Length of broadcast_dimensions must equal rank of operand")
  }

  # (C3) 0 <= broadcast_dimensions < rank(result)
  if (length(result_dims) == 0L) {
    cli::cli_abort(
      "shape_out must specify the full result shape (rank > 0 for non-scalars)"
    )
  }
  if (any(bdims < 0L | bdims >= length(result_dims))) {
    cli::cli_abort("broadcast_dimensions must be within [0, rank(result))")
  }

  # (C4) is_unique(broadcast_dimensions)
  if (any(duplicated(bdims))) {
    cli::cli_abort("broadcast_dimensions must be unique")
  }

  # (C5) For all d in axes(operand):
  #   shape(operand, d) = 1 OR shape(operand, d) = shape(result, broadcast_dimensions[d])
  for (d in seq_along(operand_dims)) {
    op_dim <- operand_dims[d]
    res_dim <- result_dims[bdims[d] + 1L] # 0-based to 1-based

    # Allow unknown dims (NA) to pass checks where appropriate
    if (op_dim != 1L && op_dim != res_dim) {
      cli::cli_abort(
        "Operand dimension and result dimension must match unless operand dim is 1"
      )
    }
  }

  ValueTypes(list(
    ValueType(
      TensorType(
        elt_type = operand@type@elt_type,
        shape = Shape(result_dims)
      )
    )
  ))
}

hlo_broadcast_in_dim_impl <- hlo_fn(
  BroadcastInDim,
  infer_types_broadcast_in_dim
)

#' @templateVar mnemonic broadcast_in_dim
#' @template op
#' @export
hlo_broadcast_in_dim <- function(
  operand,
  broadcast_dimensions,
  shape_out
) {
  bd_attr <- hlo_tensor(as.integer(broadcast_dimensions), elt_type = "i64")

  hlo_broadcast_in_dim_impl(
    values = list(operand = operand),
    attrs = list(broadcast_dimensions = bd_attr),
    custom_attrs = list(shape_out = as.integer(shape_out))
  )
}

method(repr, BroadcastInDim) <- function(x) {
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
