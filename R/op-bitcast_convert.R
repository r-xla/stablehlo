#' @include op.R hlo.R
NULL

OpBitcastConvert <- new_Op("OpBitcastConvert", "bitcast_convert")

#' @rdname hlo_bitcast_convert
#' @export
infer_types_bitcast_convert <- function(
  operand,
  dtype
) {
  assert_vt_is_tensor(operand)

  if (
    # https://github.com/openxla/stablehlo/issues/1672
    dtype %in%
      c("i1", "pred") ||
      (test_class(operand$type$dtype, "BooleanType"))
  ) {
    cli_abort("Bitcast conversions from and to booleans are not supported.")
  }

  if (
    !(dtype %in%
      c(
        "i8",
        "i16",
        "i32",
        "i64",
        "ui8",
        "ui16",
        "ui32",
        "ui64",
        "f8",
        "f16",
        "f32",
        "f64"
      ))
  ) {
    cli_abort("Unsupported dtype: {dtype}")
  }

  operand_bits <- operand$type$dtype$value
  operand_dims <- shape(operand)

  output_bits <- as.integer(sub(".*?([0-9]+)$", "\\1", dtype))
  cst_fct <- output_bits / operand_bits

  if (cst_fct == 1) {
    result_dims <- operand_dims
  } else if (cst_fct > 1) {
    if (identical(operand_dims, integer(0))) {
      cli_abort(
        "{.var operand} must have at least 1 dimension for this bitcast conversion."
      )
    } else if (operand_dims[[length(operand_dims)]] != cst_fct) {
      cli_abort(
        "The last dimension of {.var operand} must be {cst_fct} for this bitcast conversion.",
        i = "Got {operand_dims[[length(operand_dims)]]}."
      )
    } else {
      result_dims <- operand_dims[seq_len(length(operand_dims) - 1)]
    }
  } else {
    result_dims <- c(operand_dims, as.integer(1 / cst_fct))
  }

  ValueTypes(list(
    ValueType(
      type = dtype,
      shape = result_dims
    )
  ))
}

hlo_bitcast_convert_impl <- hlo_fn(
  OpBitcastConvert,
  infer_types_bitcast_convert
)

#' @templateVar mnemonic bitcast_convert
#' @template op
#' @export
hlo_bitcast_convert <- function(
  operand,
  dtype
) {
  hlo_bitcast_convert_impl(
    values = list(operand = operand),
    custom_attrs = list(dtype = dtype)
  )
}
