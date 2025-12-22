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

  # (C1) element_type(operand) = element_type(padding_value) = element_type(result)
  assert_vts_have_same_dtype(operand, padding_value)

  operand_shape <- shape(operand)
  operand_rank <- length(operand_shape)

  low <- edge_padding_low@value@data
  high <- edge_padding_high@value@data
  interior <- interior_padding@value@data

  lowhigh <- rbind(low, high)
  lowhigh[lowhigh > 0] <- 0
  if (any(colSums(abs(lowhigh)) > operand_rank)) {
    cli_abort("negative padding values can't exceed dimension")
  }

  if (any(interior < 0)) {
    cli_abort("interior_padding must be non-negative")
  }

  # (C2) size(edge_padding_low) = size(edge_padding_high) = size(interior_padding) = rank(operand)
  if (length(low) != operand_rank) {
    cli_abort("edge_padding_low must have length equal to operand rank")
  }
  if (length(high) != operand_rank) {
    cli_abort("edge_padding_high must have length equal to operand rank")
  }
  if (length(interior) != operand_rank) {
    cli_abort("interior_padding must have length equal to operand rank")
  }

  # (C3) 0 <= interior_padding
  if (any(interior < 0)) {
    cli_abort("interior_padding must be non-negative")
  }

  # (C4) shape(result) = shape(operand) + edge_padding_low +
  #      max(shape(operand) - 1, 0) * interior_padding + edge_padding_high
  result_shape <- operand_shape +
    low +
    pmax(operand_shape - 1L, 0L) * interior +
    high

  ValueTypes(list(
    ValueType(
      TensorType(
        dtype = operand@type@dtype,
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
        dtype = "i64"
      ),
      constant_attr(
        "edge_padding_high",
        as.integer(edge_padding_high),
        dtype = "i64"
      ),
      constant_attr(
        "interior_padding",
        as.integer(interior_padding),
        dtype = "i64"
      )
    )
  )
}
