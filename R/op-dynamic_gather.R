#' @include op.R hlo.R op-gather.R
NULL

OpDynamicGather <- new_Op("OpDynamicGather", "dynamic_gather")

#' @rdname hlo_dynamic_gather
#' @param slice_sizes ([`FuncValue`] | [`ValueType`])\cr
#'   A rank-1 integer tensor giving the slice extents. A *value*, which is what
#'   distinguishes this op from [`hlo_gather()`].
#' @export
infer_types_dynamic_gather <- function(
  operand,
  start_indices,
  slice_sizes,
  gather_dimension_numbers,
  indices_are_sorted = FALSE
) {
  assert_vt_is_tensor(slice_sizes)
  declared <- shape(slice_sizes)
  if (length(declared) != 1L) {
    cli_abort(c(
      "{.arg slice_sizes} must be a rank-1 tensor.",
      x = "Got shape {shapevec_repr(declared)}."
    ))
  }
  rank <- length(shape(operand))
  # (C8) one slice extent per axis of `operand`.
  if (must_ne(declared, rank)) {
    cli_abort(c(
      "{.arg slice_sizes} must have one element per axis of {.arg operand}.",
      x = "Got {vec_repr(declared)} elements for a rank-{rank} operand."
    ))
  }

  # Everything else this op constrains, `gather` already constrains the same
  # way -- the only difference is where `slice_sizes` comes from. So the static
  # inference runs with the extents marked unknown: every check that does not
  # depend on them still fires, the ones that do defer, and the result shape
  # comes back with `?` exactly on the axes the extents determine.
  infer_types_gather(
    operand = operand,
    start_indices = start_indices,
    gather_dimension_numbers = gather_dimension_numbers,
    slice_sizes = r_to_constant(
      rep(NA_integer_, rank),
      dtype = "i64",
      shape = rank
    ),
    indices_are_sorted = indices_are_sorted
  )
}

hlo_dynamic_gather_impl <- hlo_fn(OpDynamicGather, infer_types_dynamic_gather)

#' @templateVar mnemonic dynamic_gather
#' @template op
#' @param gather_dimension_numbers ([`GatherDimensionNumbers`])\cr
#'   Which axes of the operand and indices play which role.
#' @param indices_are_sorted (`logical(1)`)\cr
#'   Whether `start_indices` is sorted; a promise, not a check.
#' @export
hlo_dynamic_gather <- function(
  operand,
  start_indices,
  slice_sizes,
  gather_dimension_numbers,
  indices_are_sorted = FALSE,
  output_types = NULL
) {
  assert_class(gather_dimension_numbers, "GatherDimensionNumbers")
  hlo_dynamic_gather_impl(
    values = list(
      operand = operand,
      start_indices = start_indices,
      slice_sizes = slice_sizes
    ),
    output_types = output_types,
    custom_attrs = list(gather_dimension_numbers = gather_dimension_numbers),
    attrs = list(
      BoolAttr(
        name = "indices_are_sorted",
        value = as.logical(indices_are_sorted)
      )
    )
  )
}
