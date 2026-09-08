#' @include op.R hlo.R op-gather.R
NULL

# `dimension_numbers` is a custom attribute, which the default renderer does
# not emit -- so this op needs a renderer of its own, exactly as `gather` does.
render_dynamic_gather <- function(ctx) {
  attrs_str <- paste(vapply(ctx$attrs, repr, character(1)), collapse = ", ")
  paste0(
    ctx$outputs_str,
    " = \"stablehlo.dynamic_gather\"(",
    ctx$values_str,
    ") {\n",
    "dimension_numbers = ",
    repr(ctx$custom_attrs$gather_dimension_numbers),
    ",\n",
    attrs_str,
    "\n}: ",
    ctx$sig_str
  )
}

OpDynamicGather <- new_Op(
  "OpDynamicGather",
  "dynamic_gather",
  render = render_dynamic_gather
)

#' @rdname hlo_dynamic_gather
#' @export
infer_types_dynamic_gather <- function(
  operand,
  start_indices,
  gather_dimension_numbers,
  slice_sizes,
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
  # (C11) `size(slice_sizes) = rank(operand)`.
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
#' @templateVar not_func_variables gather_dimension_numbers,indices_are_sorted
#' @template op
#' @param gather_dimension_numbers ([`GatherDimensionNumbers`])\cr
#'   Which axes of the operand and indices play which role.
#' @param indices_are_sorted (`logical(1)`)\cr
#'   Whether `start_indices` is sorted; a promise, not a check.
#' @export
hlo_dynamic_gather <- function(
  operand,
  start_indices,
  gather_dimension_numbers,
  slice_sizes,
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
