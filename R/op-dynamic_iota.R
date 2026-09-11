#' @include op.R hlo.R
NULL

OpDynamicIota <- new_Op("OpDynamicIota", "dynamic_iota")

#' @rdname hlo_dynamic_iota
#' @param shape (`integer()`)\cr
#'   The result's static shape, with `NA` at each axis whose size is only known
#'   at run time. It cannot be inferred, because `output_shape` is data.
#' @export
infer_types_dynamic_iota <- function(
  output_shape,
  iota_dimension,
  dtype,
  shape
) {
  assert_vt_is_tensor(output_shape)
  assert_const(iota_dimension, dtype = as_dtype("i64"), shape = integer())
  assert_shapevec_dyn(shape)
  shape <- as.integer(shape)

  iota_dim <- as.integer(iota_dimension$data)

  # (C1) `0 <= iota_dimension < size(output_shape)`. `size(output_shape)` is
  # the result's rank, which the hint gives us; the tensor's own extent says
  # the same thing and is checked against it below.
  if (iota_dim < 0L || iota_dim >= length(shape)) {
    error_index_out_of_bounds(
      arg = "iota_dimension",
      index = iota_dim,
      lower = 0L,
      upper = length(shape)
    )
  }

  # (C2) `rank(result) = size(output_shape)`, plus I1's type.
  assert_size_operand(output_shape, length(shape))

  dtype <- as_dtype(dtype)
  assert_dtype_one_of(dtype, c("int", "uint", "float"))

  ValueTypes(list(ValueType(TensorType(dtype = dtype, shape = Shape(shape)))))
}

hlo_dynamic_iota_impl <- hlo_fn(OpDynamicIota, infer_types_dynamic_iota)

#' @templateVar mnemonic dynamic_iota
#' @templateVar not_func_variables iota_dimension,dtype,shape
#' @template op
#' @details
#' Note that `shape` is a *claim*, not a check: nothing here can verify it,
#' since the sizes it describes are data. Where StableHLO can constant-fold the
#' size operands it will verify the claim itself and reject a wrong one
#' downstream.
#' @param iota_dimension (`integer(1)`)\cr
#'   The axis along which to generate increasing values.
#' @param dtype (`character(1)`)\cr
#'   The data type of the result.
#' @export
hlo_dynamic_iota <- function(
  output_shape,
  iota_dimension,
  dtype,
  shape,
  output_types = NULL
) {
  hlo_dynamic_iota_impl(
    values = list(output_shape = output_shape),
    output_types = output_types,
    attrs = list(
      ScalarAttr(
        name = "iota_dimension",
        value = as.integer(iota_dimension),
        dtype = as_dtype("i64")
      )
    ),
    custom_attrs = list(dtype = as_dtype(dtype), shape = as.integer(shape))
  )
}
