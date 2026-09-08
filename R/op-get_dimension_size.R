#' @include op.R hlo.R
NULL

OpGetDimensionSize <- new_Op("OpGetDimensionSize", "get_dimension_size")

#' @rdname hlo_get_dimension_size
#' @export
infer_types_get_dimension_size <- function(operand, dimension) {
  assert_vt_is_tensor(operand)
  assert_const(dimension, dtype = as_dtype("i64"), shape = integer())

  dims <- shape(operand)
  d <- dimension$data

  # (C1)
  if (d < 0L || d >= length(dims)) {
    error_index_out_of_bounds(
      arg = "dimension",
      index = d,
      lower = 0L,
      upper = length(dims)
    )
  }

  # The result is an i32 scalar whichever axis is read; the spec lists no
  # constraint on it, only (C1) above.
  ValueTypes(list(
    ValueType(TensorType(dtype = as_dtype("i32"), shape = Shape(integer())))
  ))
}

hlo_get_dimension_size_impl <- hlo_fn(
  OpGetDimensionSize,
  infer_types_get_dimension_size
)

#' @templateVar mnemonic get_dimension_size
#' @template op
#' @export
hlo_get_dimension_size <- function(operand, dimension, output_types = NULL) {
  hlo_get_dimension_size_impl(
    values = list(operand = operand),
    output_types = output_types,
    attrs = list(
      ScalarAttr(
        name = "dimension",
        value = as.integer(dimension),
        dtype = as_dtype("i64")
      )
    )
  )
}
