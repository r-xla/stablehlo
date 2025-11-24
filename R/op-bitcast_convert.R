#' @include op.R hlo.R
NULL

OpBitcastConvert <- new_Op("OpBitcastConvert", "bitcast_convert")

infer_types_bitcast_convert <- function(
  operand,
  cast_to_dtype
) {
  stopifnot(inherits(operand@type, TensorType))
  stopifnot(
    cast_to_dtype %in%
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

  operand_bits <- operand@type@dtype@value
  operand_dims <- shape(operand)

  if (cast_to_dtype == "pred") {
    output_bits <- 1L
  } else {
    output_bits <- as.integer(sub(".*?([0-9]+)$", "\\1", cast_to_dtype))
  }

  cst_fct <- output_bits / operand_bits

  result_dims <- operand_dims
  if (cst_fct > 1) {
    if (operand_dims[[length(operand_dims)]] %% cst_fct != 0) {
      cli_abort(
        sprintf(
          "when upcasting, operands last dimension must be divisible by the upcasting factor (%d)",
          cst_fct
        )
      )
    }
    result_dims[[length(result_dims)]] <- as.integer(
      result_dims[[length(result_dims)]] / cst_fct
    )
  } else {
    result_dims <- c(result_dims, as.integer(round(1 / cst_fct)))
  }

  ValueTypes(list(
    ValueType(
      type = cast_to_dtype,
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
  cast_to_dtype
) {
  hlo_bitcast_convert_impl(
    values = list(operand = operand),
    custom_attrs = list(cast_to_dtype = cast_to_dtype)
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
