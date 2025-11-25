#' @include op.R hlo.R
NULL

OpBitcastConvert <- new_Op("OpBitcastConvert", "bitcast_convert")

infer_types_bitcast_convert <- function(
  operand,
  dtype
) {
  stopifnot(inherits(operand@type, TensorType))
  stopifnot(
    dtype %in%
      c(
        "pred",
        "i1",
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
      )
  )

  if (inherits(operand@type@dtype, BooleanType)) {
    operand_bits <- 1
  } else {
    operand_bits <- operand@type@dtype@value
  }
  operand_dims <- shape(operand)

  if (dtype == "pred") {
    output_bits <- 1L
  } else {
    output_bits <- as.integer(sub(".*?([0-9]+)$", "\\1", dtype))
  }

  cst_fct <- output_bits / operand_bits

  result_dims <- operand_dims
  if (cst_fct > 1) {
    if (operand_dims[[length(operand_dims)]] != cst_fct) {
      sprintf(
        "when upcasting, operands last dimension must be identical to the cast-factor (%d), but is %d",
        cst_fct,
        operand_dims[[length(operand_dims)]]
      )
    } else {
      result_dims <- result_dims[seq_len(length(result_dims) - 1)]
    }
  } else {
    result_dims <- c(result_dims, as.integer(1 / cst_fct))
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

method(repr, OpBitcastConvert) <- function(x) {
  paste0(
    repr(x@outputs),
    " = ",
    repr(x@name),
    " ",
    repr(x@inputs, simplify_dense = TRUE),
    ": ",
    repr(x@signature)
  )
}
