#' @include op.R hlo.R
NULL

OpPad <- new_Op("OpPad", "pad")

#' @rdname hlo_pad
#' @export
infer_types_pad <- function(
  operand,
  padding_value,
  edge_padding_low,
  edge_padding_high,
  interior_padding
) {
  assert_vts_are_tensors(operand, padding_value)

  # (I2): padding_value must be a 0-dimensional tensor.
  if (length(shape(padding_value)) != 0L) {
    cli_abort(c(
      "{.arg padding_value} must be a 0-dimensional tensor.",
      x = "Got shape {shapevec_repr(shape(padding_value))}."
    ))
  }

  # (C1)
  assert_vts_have_same_dtype(operand, padding_value)

  operand_shape <- shape(operand)
  operand_rank <- length(operand_shape)

  low <- edge_padding_low$data
  high <- edge_padding_high$data
  interior <- interior_padding$data

  # (C3)
  if (any(interior < 0)) {
    cli_abort(c(
      "interior_padding must be non-negative",
      x = "interior_padding: {vec_repr(interior)}"
    ))
  }
  # (C2)
  check <- function(val, name) {
    if (length(val) != operand_rank) {
      cli_abort(c(
        "{name} must have length equal to operand rank",
        x = "length({name}): {length(val)}, operand_rank: {operand_rank}"
      ))
    }
  }
  check(low, "edge_padding_low")
  check(high, "edge_padding_high")
  check(interior, "interior_padding")

  # (C4)
  result_shape <- operand_shape +
    low +
    pmax(operand_shape - 1L, 0L) * interior +
    high

  # A shape cannot be negative, so (C4) is also the negative-padding check --
  # per axis, against that axis's size, and deferred where the axis is dynamic
  # (a `?` may well be large enough to absorb the trimming). It has to come
  # after (C2) so the four vectors line up.
  if (any(provably_gt(0L, result_shape))) {
    axes <- which(provably_gt(0L, result_shape))
    cli_abort(c(
      "Negative padding must not remove more than an axis holds.",
      x = "Axis {axes - 1L} of {.arg operand} has size {operand_shape[axes]}, and the padding leaves {result_shape[axes]}."
    ))
  }

  ValueTypes(list(
    ValueType(
      TensorType(
        dtype = operand$type$dtype,
        shape = Shape(result_shape)
      )
    )
  ))
}

hlo_pad_impl <- hlo_fn(
  OpPad,
  infer_types_pad
)

#' @templateVar mnemonic pad
#' @template op
#' @export
hlo_pad <- function(
  operand,
  padding_value,
  edge_padding_low,
  edge_padding_high,
  interior_padding,
  output_types = NULL
) {
  hlo_pad_impl(
    values = list(operand = operand, padding_value = padding_value),
    output_types = output_types,
    attrs = list(
      constant_attr(
        "edge_padding_low",
        as.integer(edge_padding_low),
        dtype = "i64",
        shape = c()
      ),
      constant_attr(
        "edge_padding_high",
        as.integer(edge_padding_high),
        dtype = "i64",
        shape = c()
      ),
      constant_attr(
        "interior_padding",
        as.integer(interior_padding),
        dtype = "i64",
        shape = c()
      )
    )
  )
}
