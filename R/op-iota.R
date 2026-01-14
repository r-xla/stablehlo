#' @include op.R hlo.R
NULL

OpIota <- new_Op("OpIota", "iota")

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
  assert_one_of(
    dtype,
    c("IntegerType", "UnsignedType", "FloatType")
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
  op <- OpIota(
    inputs = OpInputs(
      values = OpInputValues(list()),
      funcs = OpInputFuncs(),
      attrs = OpInputAttrs(
        list(
          ScalarAttr(
            name = "iota_dimension",
            value = as.integer(iota_dimension_const$data),
            dtype = IntegerType(64L)
          )
        )
      )
    ),
    outputs = OpOutputs(
      items = list(
        OpOutput(value_id)
      )
    ),
    signature = OpSignature(
      input_types = ValueTypes(list()),
      output_types = output_types
    )
  )
  func$body <- FuncBody(c(func$body, list(op)))

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
#'   The element type of the output tensor.
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

globals[["infer_fn"]][["iota"]] <- infer_types_iota
