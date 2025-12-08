#' @include op.R hlo.R type_inference.R
NULL

OpConvert <- new_Op("OpConvert", "convert")

#' @rdname hlo_convert
#' @export
infer_types_convert <- function(operand, dtype) {
  assert_vt_is_tensor(operand)
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
hlo_convert <- function(operand, dtype) {
  hlo_convert_impl(
    values = list(operand = operand),
    custom_attrs = list(dtype = dtype)
  )
}
