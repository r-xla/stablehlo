#' @include op.R hlo.R
NULL

OpClamp <- new_Op("OpClamp", "clamp")

#' @rdname hlo_clamp
#' @export
# binary ops
infer_types_clamp <- function(min, operand, max) {
  assert_vts_are_tensors(min = min, operand = operand, max = max)

  # (C3)
  assert_vts_have_same_dtype(operand, max)
  assert_vts_have_same_dtype(min, operand)

  min_shape <- shape(min)
  operand_shape <- shape(operand)
  max_shape <- shape(max)

  # (C1)
  if (length(min_shape) != 0L && !identical(min_shape, operand_shape)) {
    cli_abort(c(
      "min must have the same shape as operand or be a scalar.",
      i = "Got min shape {shapevec_repr(min_shape)} and operand shape {shapevec_repr(max_shape)}."
    ))
  }

  # (C1)
  if (length(max_shape) != 0L && !identical(max_shape, operand_shape)) {
    cli_abort(c(
      "max must have the same shape as operand or be a scalar.",
      i = "Got max shape {shapevec_repr(min_shape)} and operand shape {shapevec_repr(max_shape)}."
    ))
  }

  # (C4)
  ValueTypes(list(operand))
}

hlo_clamp_impl <- hlo_fn(OpClamp, infer_types_clamp)

#' @templateVar mnemonic clamp
#' @template op
#' @export
hlo_clamp <- function(min, operand, max) {
  hlo_clamp_impl(values = list(min = min, operand = operand, max = max))
}
