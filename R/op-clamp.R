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

  # (C1) A bound is either a scalar or shaped like the operand. `identical()`
  # is too strong once a size can be dynamic -- it would reject a
  # `tensor<?xf32>` bound against a `tensor<3xf32>` operand, which agree
  # whenever the bound turns out to hold 3 -- so the rank must match exactly
  # (never dynamic) and the sizes only have to be possible.
  # `call` so the error is reported against infer_types_clamp() rather than
  # this local helper -- callers (anvl) rewrite these errors and key on the
  # call.
  check_bound <- function(bound_shape, arg, call = rlang::caller_env()) {
    if (length(bound_shape) == 0L) {
      return(invisible(NULL)) # a scalar bound broadcasts
    }
    same <- length(bound_shape) == length(operand_shape) &&
      !any(provably_ne(bound_shape, operand_shape))
    if (!same) {
      cli_abort(
        c(
          "{.arg {arg}} must have the same shape as {.arg operand} or be a scalar.",
          x = "Got shapes {shapevec_repr(bound_shape)} and {shapevec_repr(operand_shape)}."
        ),
        call = call
      )
    }
    invisible(NULL)
  }
  check_bound(min_shape, "min")
  check_bound(max_shape, "max")

  # (C4) The result takes the most refined shape the three operands agree on,
  # so a dynamic operand clamped by static bounds yields a static result.
  #
  # `check_bound()` above compares each bound to the operand, which leaves the
  # bounds themselves unchecked against each other: with a dynamic operand,
  # `min` of 3 and `max` of 5 pass both checks and only clash here. Reported
  # in clamp's own words rather than `unify_shapes()`'s, whose message would name
  # an argument clamp does not have.
  result_shape <- operand_shape
  for (nm in c("min", "max")) {
    bound_shape <- if (identical(nm, "min")) min_shape else max_shape
    if (length(bound_shape) == 0L) {
      next
    }
    if (any(provably_ne(result_shape, bound_shape))) {
      cli_abort(c(
        "{.arg min}, {.arg max} and {.arg operand} must have the same shape.",
        x = "Got {shapevec_repr(operand_shape)}, {shapevec_repr(min_shape)} and {shapevec_repr(max_shape)}."
      ))
    }
    result_shape <- unify_shapes(
      result_shape,
      bound_shape,
      arg_a = "operand",
      arg_b = nm
    )
  }

  ValueTypes(list(
    ValueType(
      TensorType(dtype = operand$type$dtype, shape = Shape(result_shape))
    )
  ))
}

hlo_clamp_impl <- hlo_fn(OpClamp, infer_types_clamp)

#' @templateVar mnemonic clamp
#' @template op
#' @export
hlo_clamp <- function(min, operand, max, output_types = NULL) {
  hlo_clamp_impl(
    values = list(min = min, operand = operand, max = max),
    output_types = output_types
  )
}
