#' @include op.R hlo.R
NULL

OpClamp <- new_Op("OpClamp", "clamp")

#' @rdname hlo_clamp
#' @export
# binary ops
infer_types_clamp <- function(Min, operand, Max) {
  assert_vts_are_tensors(Min = Min, operand = operand, Max = Max)

  # (C3)
  assert_vts_have_same_dtype(operand, Max)
  assert_vts_have_same_dtype(Min, operand)

  min_shape <- shape(Min)
  operand_shape <- shape(operand)
  max_shape <- shape(Max)

  # (C1)
  if (length(min_shape) != 0L && !identical(min_shape, operand_shape)) {
    cli_abort(c(
      "Min must have the same shape as operand or be a scalar.",
      i = "Got Min shape {.val {min_shape}} and operand shape {.val {operand_shape}}."
    ))
  }

  # (C1)
  if (length(max_shape) != 0L && !identical(max_shape, operand_shape)) {
    cli_abort(c(
      "Max must have the same shape as operand or be a scalar.",
      i = "Got Max shape {.val {max_shape}} and operand shape {.val {operand_shape}}."
    ))
  }

  # (C4)
  ValueTypes(list(operand))
}

hlo_clamp_impl <- hlo_fn(OpClamp, infer_types_clamp)

#' @templateVar mnemonic clamp
#' @template op
#' @export
hlo_clamp <- function(Min, operand, Max) {
  hlo_clamp_impl(values = list(Min = Min, operand = operand, Max = Max))
}
