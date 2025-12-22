#' @include op.R
NULL

OpIota <- new_Op("OpIota", "iota")
impl_hlo_iota <- function(iota_dimension, dtype, shape, func) {
  iota_dimension <- assert_int(iota_dimension, coerce = TRUE)
  shape <- as.integer(shape)

  # (C1) 0 <= iota_dimension < rank(output)
  rank <- length(shape)
  if (iota_dimension < 0L || iota_dimension >= rank) {
    cli_abort(
      "iota_dimension must be in range [0, {rank}), got {iota_dimension}"
    )
  }

  out_dtype <- as_dtype(dtype)
  # iota supports integer, floating-point, or complex types (we don't support complex)
  assert_one_of(out_dtype, IntegerType, UnsignedType, FloatType)

  output_type <- ValueType(
    TensorType(
      dtype = out_dtype,
      shape = Shape(shape)
    )
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
            value = iota_dimension,
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
      output_types = ValueTypes(list(output_type))
    )
  )
  func@body <- FuncBody(c(func@body@items, list(op)))

  FuncValue(
    value_id = value_id,
    value_type = output_type,
    func = func
  )
}

#' @templateVar mnemonic iota
#' @template op
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
hlo_iota <- function(iota_dimension, dtype, shape, func = NULL) {
  func <- func %??% .current_func()
  impl_hlo_iota(iota_dimension, dtype, shape, func)
}

#' @rdname hlo_iota
#' @export
infer_types_iota <- function(iota_dimension, dtype, shape) {
  iota_dimension <- assert_int(iota_dimension, coerce = TRUE)
  shape <- as.integer(shape)

  rank <- length(shape)
  if (iota_dimension < 0L || iota_dimension >= rank) {
    cli_abort(
      "iota_dimension must be in range [0, {rank}), got {iota_dimension}"
    )
  }

  out_dtype <- as_dtype(dtype)
  assert_one_of(out_dtype, IntegerType, UnsignedType, FloatType)

  ValueTypes(list(
    ValueType(
      TensorType(
        dtype = out_dtype,
        shape = Shape(shape)
      )
    )
  ))
}

method(repr, OpIota) <- function(x, ...) {
  paste0(
    repr(x@outputs),
    " = ",
    repr(x@name),
    " ",
    repr(x@inputs),
    ": ",
    repr(x@signature)
  )
}
