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

  # No `naxes`: a rank-1 operand may be padded with rank-0 constants, and the
  # length against the operand's rank is checked below.
  assert_const(edge_padding_low, dtype = as_dtype("i64"))
  assert_const(edge_padding_high, dtype = as_dtype("i64"))
  assert_const(interior_padding, dtype = as_dtype("i64"))

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

  # (C4) In double: integer arithmetic overflows to `NA` for a padding that is
  # large but inside the integer range on its own, and the test below then
  # reaches `if ()` with a missing value.
  result_shape <- as.double(operand_shape) +
    low +
    pmax(as.double(operand_shape) - 1, 0) * interior +
    high

  # (C4) Negative edge padding removes elements, and may not remove more than
  # the dimension it applies to holds: every dimension of the result is a size,
  # so a negative one is not a shape at all. Checked here rather than left to
  # `Shape()`, which would refuse it without naming an argument.
  if (any(result_shape < 0L)) {
    cli_abort(c(
      "{.arg edge_padding_low} and {.arg edge_padding_high} must not remove more elements than a dimension holds.", # nolint
      x = "Padding {.arg operand} of shape {shapevec_repr(operand_shape)} by {vec_repr(low)} and {vec_repr(high)} would give {vec_repr(result_shape)}." # nolint
    ))
  }

  result_shape <- assert_result_dims(result_shape, "The padded tensor")

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
