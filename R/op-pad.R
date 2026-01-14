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

  # (C1)
  assert_vts_have_same_dtype(operand, padding_value)

  operand_shape <- shape(operand)
  operand_rank <- length(operand_shape)

  low <- edge_padding_low$data
  high <- edge_padding_high$data
  interior <- interior_padding$data

  lowhigh <- rbind(low, high)
  lowhigh[lowhigh > 0] <- 0
  if (any(colSums(abs(lowhigh)) > operand_rank)) {
    cli_abort(c(
      "negative padding values can't exceed dimension",
      i = "edge_padding_low: {.val {low}}, edge_padding_high: {.val {high}}, operand_rank: {operand_rank}"
    ))
  }

  # (C3)
  if (any(interior < 0)) {
    cli_abort(c(
      "interior_padding must be non-negative",
      i = "interior_padding: {.val {interior}}"
    ))
  }
  # (C2)
  check <- function(val, name) {
    if (length(val) != operand_rank) {
      cli_abort(c(
        "{name} must have length equal to operand rank",
        i = "length({name}): {length(val)}, operand_rank: {operand_rank}"
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
  interior_padding
) {
  hlo_pad_impl(
    values = list(operand = operand, padding_value = padding_value),
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
