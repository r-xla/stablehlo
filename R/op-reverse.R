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
  assert_const(dimensions, dtype = IntegerType(64L), ndims = 1L)

  operand_dims <- shape(operand)
  revdims <- dimensions$data

  # (C2) is_unique(dimensions).
  if (anyDuplicated(revdims) > 0) {
    error_dimension_uniqueness(
      arg = "dimensions",
      dimensions = revdims
    )
  }

  # (C3) 0 <= dimensions < rank(result)
  if (length(revdims) == 0L) {
    cli_abort(
      "at least one dimension needs to be provided"
    )
  }
  if (any(revdims < 0L | revdims >= length(operand_dims))) {
    error_dimension_out_of_range(
      arg = "dimensions",
      dimension = revdims,
      dim_range = c(0L, length(operand_dims))
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
  dimensions
) {
  hlo_reverse_impl(
    values = list(operand = operand),
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
