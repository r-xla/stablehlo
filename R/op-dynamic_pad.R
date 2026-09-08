#' @include op.R hlo.R
NULL

OpDynamicPad <- new_Op("OpDynamicPad", "dynamic_pad")

#' @rdname hlo_dynamic_pad
#' @param padding_value ([`FuncValue`] | [`ValueType`])\cr
#'   A scalar of the operand's element type, used for the padding.
#' @param edge_padding_low,edge_padding_high,interior_padding ([`FuncValue`] |
#'   [`ValueType`])\cr
#'   Rank-1 integer tensors, one element per axis of `operand`. These are
#'   values rather than attributes, which is what distinguishes this op from
#'   [`hlo_pad()`].
#' @param shape (`integer()`)\cr
#'   The result's static shape, with `NA` at each axis whose size is only known
#'   at run time. It cannot be inferred, because the paddings are data.
#' @export
infer_types_dynamic_pad <- function(
  operand,
  padding_value,
  edge_padding_low,
  edge_padding_high,
  interior_padding,
  shape
) {
  assert_vts_are_tensors(
    operand = operand,
    padding_value = padding_value,
    edge_padding_low = edge_padding_low,
    edge_padding_high = edge_padding_high,
    interior_padding = interior_padding
  )
  assert_shapevec_dyn(shape)
  shape <- as.integer(shape)

  # (C1) The element types agree; `padding_value` is a scalar.
  if (length(shape(padding_value)) != 0L) {
    cli_abort(c(
      "{.arg padding_value} must be a 0-dimensional tensor.",
      x = "Got shape {shapevec_repr(shape(padding_value))}."
    ))
  }
  if (operand$type$dtype != padding_value$type$dtype) {
    cli_abort(c(
      "{.arg operand} and {.arg padding_value} must have the same data type.",
      x = "Got {.val {operand$type}} and {.val {padding_value$type}}."
    ))
  }

  # (C2) Each padding vector has one element per axis of `operand`.
  rank <- length(shape(operand))
  paddings <- list(
    edge_padding_low = edge_padding_low,
    edge_padding_high = edge_padding_high,
    interior_padding = interior_padding
  )
  for (nm in names(paddings)) {
    declared <- shape(paddings[[nm]])
    if (length(declared) != 1L) {
      cli_abort(c(
        "{.arg {nm}} must be a rank-1 tensor.",
        x = "Got shape {shapevec_repr(declared)}."
      ))
    }
    if (must_ne(declared, rank)) {
      cli_abort(c(
        "{.arg {nm}} must have one element per axis of {.arg operand}.",
        x = "Got {vec_repr(declared)} elements for a rank-{rank} operand."
      ))
    }
  }

  # (C4) The result's shape is a function of the paddings, which are data, so
  # it is taken from the hint rather than computed. Its rank is still the
  # operand's.
  if (length(shape) != rank) {
    cli_abort(c(
      "The result must have the same rank as {.arg operand}.",
      x = "Got {shapevec_repr(shape)} for a rank-{rank} operand."
    ))
  }

  ValueTypes(list(
    ValueType(TensorType(dtype = operand$type$dtype, shape = Shape(shape)))
  ))
}

hlo_dynamic_pad_impl <- hlo_fn(OpDynamicPad, infer_types_dynamic_pad)

#' @templateVar mnemonic dynamic_pad
#' @template op
#' @export
hlo_dynamic_pad <- function(
  operand,
  padding_value,
  edge_padding_low,
  edge_padding_high,
  interior_padding,
  shape,
  output_types = NULL
) {
  hlo_dynamic_pad_impl(
    values = list(
      operand = operand,
      padding_value = padding_value,
      edge_padding_low = edge_padding_low,
      edge_padding_high = edge_padding_high,
      interior_padding = interior_padding
    ),
    output_types = output_types,
    custom_attrs = list(shape = as.integer(shape))
  )
}
