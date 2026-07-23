#' @include op.R hlo.R
NULL

#' @rdname hlo_iota
#' @export
infer_types_iota <- function(iota_dimension, dtype, shape) {
  assert_const(iota_dimension, dtype = "i64", shape = c())
  shape <- as.integer(shape)

  iota_dim <- as.integer(iota_dimension$data)

  # (C1)
  num_dims <- length(shape)
  if (iota_dim < 0L || iota_dim >= num_dims) {
    error_index_out_of_bounds(
      arg = "iota_dimension",
      index = iota_dim,
      lower = 0L,
      upper = num_dims
    )
  }

  dtype <- as_dtype(dtype)
  assert_dtype_one_of(
    dtype,
    c("int", "uint", "float")
  )

  ValueTypes(list(
    ValueType(
      TensorType(
        dtype = dtype,
        shape = Shape(shape)
      )
    )
  ))
}

impl_hlo_iota <- function(iota_dimension, dtype, shape, func) {
  iota_dimension_const <- r_to_constant(
    as.integer(iota_dimension),
    dtype = "i64",
    shape = c()
  )

  # Run type inference
  output_types <- infer_types_iota(
    iota_dimension = iota_dimension_const,
    dtype = dtype,
    shape = shape
  )

  value_id <- ValueId()
  attr_str <- render_attrs(list(
    ScalarAttr(
      name = "iota_dimension",
      value = as.integer(iota_dimension_const$data),
      dtype = as_dtype("i64")
    )
  ))
  func_emit(
    func,
    list(
      function(p) {
        paste0(
          repr(p$value_id),
          " = \"stablehlo.iota\" ()",
          p$attr_str,
          ": () -> (",
          p$out_type_str,
          ")"
        )
      },
      list(
        value_id = value_id,
        attr_str = attr_str,
        out_type_str = type_str(output_types[[1L]])
      )
    )
  )

  FuncValue(
    value_id = value_id,
    value_type = output_types[[1L]],
    func = func
  )
}

#' @title Iota Operator
#' @description
#' See \url{https://openxla.org/stablehlo/spec#iota} for details.
#' @param iota_dimension (`integer(1)`)\cr
#'   The dimension along which to generate increasing values.
#'   Must be in range `[0, rank(output))`.
#' @param dtype (`character(1)`)\cr
#'   The data type of the output tensor.
#'   One of: `r roxy_dtypes()` (excluding boolean).
#' @param shape (`integer()`)\cr
#'   The shape of the output tensor.
#' @param func ([`Func`])\cr
#'   The function to add the operation to.
#'   Per default, uses the last function created with [`hlo_func`] or [`local_func`].
#' @export
#' @return [`FuncValue`]
hlo_iota <- function(iota_dimension, dtype, shape, func = NULL) {
  func <- func %??% .current_func()
  impl_hlo_iota(iota_dimension, dtype, shape, func)
}
