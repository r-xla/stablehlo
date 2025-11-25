#' @include op.R hlo.R
NULL

OpReverse <- new_Op("OpReverse", "reverse")

#' @rdname hlo_reverse
#' @export
infer_types_reverse <- function(
  operand,
  dimensions
) {
  stopifnot(inherits(operand@type, TensorType))

  operand_dims <- shape(operand)
  revdims <- dimensions@value@data

  # (C2) is_unique(dimensions).
  if (anyDuplicated(revdims) > 0) {
    cli_abort("dimensions must be unique")
  }

  # (C3) 0 <= dimensions < rank(result)
  if (length(revdims) == 0L) {
    cli_abort(
      "at least one dimension needs to be provided"
    )
  }
  if (any(revdims < 0L | revdims >= length(operand_dims))) {
    cli_abort("dimensions must be within [0, rank(result))")
  }

  ValueTypes(list(
    ValueType(
      TensorType(
        dtype = operand@type@dtype,
        shape = Shape(operand_dims)
      )
    )
  ))
}

hlo_reverse_impl <- hlo_fn(
  OpReverse,
  infer_types_reverse
)

#' @templateVar mnemonic reverse
#' @template op
#' @export
hlo_reverse <- function(
  operand,
  dimensions
) {
  dim_attr <- hlo_tensor(as.integer(dimensions), dtype = "i64")

  hlo_reverse_impl(
    values = list(operand = operand),
    attrs = list(dimensions = dim_attr)
  )
}

method(repr, OpReverse) <- function(x) {
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
