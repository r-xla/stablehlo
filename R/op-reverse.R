#' @include op.R hlo.R
NULL

OpReverse <- new_Op("OpReverse", "reverse")

#' @rdname hlo_reverse
#' @export
infer_types_reverse <- function(
  operand,
  dimensions
) {
  assert_vt_is_tensor(operand)
  assert_const(dimensions, dtype = as_dtype("i64"), naxes = 1L)

  operand_dims <- shape(operand)
  revdims <- dimensions$data

  # (C2) is_unique(dimensions).
  if (anyDuplicated(revdims) > 0) {
    error_dimension_uniqueness(
      arg = "dimensions",
      dimensions = revdims
    )
  }

  # (C3) 0 <= dimensions < rank(result). An empty `dimensions` satisfies (C2)
  # and (C3) vacuously and StableHLO accepts it, so it is not refused here --
  # a lowering that computes the set may legitimately end up with none.
  if (any(revdims < 0L | revdims >= length(operand_dims))) {
    error_index_out_of_bounds(
      arg = "dimensions",
      index = revdims,
      lower = 0L,
      upper = length(operand_dims)
    )
  }

  ValueTypes(list(
    ValueType(
      TensorType(
        dtype = operand$type$dtype,
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
  dimensions,
  output_types = NULL
) {
  hlo_reverse_impl(
    values = list(operand = operand),
    output_types = output_types,
    attrs = list(
      constant_attr(
        "dimensions",
        as.integer(dimensions),
        shape = length(dimensions),
        dtype = "i64"
      )
    )
  )
}
