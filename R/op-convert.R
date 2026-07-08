#' @include op.R hlo.R type_inference.R
NULL

OpConvert <- new_Op("OpConvert", "convert")

#' @rdname hlo_convert
#' @export
infer_types_convert <- function(operand, dtype) {
  assert_vt_is_tensor(operand)
  # (C1)
  output_dtype <- as_dtype(dtype)
  ValueTypes(list(
    ValueType(
      TensorType(
        dtype = output_dtype,
        shape = Shape(shape(operand))
      )
    )
  ))
}

hlo_convert_impl <- hlo_fn(OpConvert, infer_types_convert)

#' @templateVar mnemonic convert
#' @template op
#' @export
hlo_convert <- function(operand, dtype, output_types = NULL) {
  hlo_convert_impl(
    values = list(operand = operand),
    output_types = output_types,
    custom_attrs = list(dtype = dtype)
  )
}
