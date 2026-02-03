#' @include op.R hlo.R type_inference.R
NULL

OpGather <- new_Op("OpGather", "gather")

#' @title GatherDimensionNumbers
#' @description
#' Represents the gather dimension numbers.
#' @param offset_dims (`integer()`)\cr
#'   The offset dimensions.
#' @param collapsed_slice_dims (`integer()`)\cr
#'   The collapsed slice dimensions.
#' @param operand_batching_dims (`integer()`)\cr
#'   The operand batching dimensions.
#' @param start_indices_batching_dims (`integer()`)\cr
#'   The start indices batching dimensions.
#' @param start_index_map (`integer()`)\cr
#'   Maps start indices to operand dimensions.
#' @param index_vector_dim (`integer(1)`)\cr
#'   The index vector dimension.
#' @export
GatherDimensionNumbers <- function(
  offset_dims,
  collapsed_slice_dims,
  operand_batching_dims = integer(),
  start_indices_batching_dims = integer(),
  start_index_map,
  index_vector_dim
) {
  assert_int(index_vector_dim)
  structure(
    list(
      offset_dims = as.integer(offset_dims),
      collapsed_slice_dims = as.integer(collapsed_slice_dims),
      operand_batching_dims = as.integer(operand_batching_dims),
      start_indices_batching_dims = as.integer(start_indices_batching_dims),
      start_index_map = as.integer(start_index_map),
      index_vector_dim = as.integer(index_vector_dim)
    ),
    class = "GatherDimensionNumbers"
  )
}

#' @export
repr.GatherDimensionNumbers <- function(x, ...) {
  parts <- c(
    sprintf(
      "offset_dims = [%s]",
      paste0(x$offset_dims, collapse = ", ")
    ),
    sprintf(
      "collapsed_slice_dims = [%s]",
      paste0(x$collapsed_slice_dims, collapse = ", ")
    )
  )

  if (
    length(x$operand_batching_dims) > 0L ||
      length(x$start_indices_batching_dims) > 0L
  ) {
    parts <- c(
      parts,
      sprintf(
        "operand_batching_dims = [%s]",
        paste0(x$operand_batching_dims, collapse = ", ")
      ),
      sprintf(
        "start_indices_batching_dims = [%s]",
        paste0(x$start_indices_batching_dims, collapse = ", ")
      )
    )
  }

  parts <- c(
    parts,
    sprintf(
      "start_index_map = [%s]",
      paste0(x$start_index_map, collapse = ", ")
    ),
    sprintf("index_vector_dim = %d", x$index_vector_dim)
  )

  sprintf(
    "#stablehlo.gather<%s>",
    paste(parts, collapse = ", ")
  )
}

#' @rdname hlo_gather
#' @export
infer_types_gather <- function(
  operand,
  start_indices,
  gather_dimension_numbers,
  slice_sizes,
  indices_are_sorted
) {
  assert_class(gather_dimension_numbers, "GatherDimensionNumbers")
  assert_const(slice_sizes, dtype = "i64")
  assert_const(indices_are_sorted, dtype = "i1", shape = integer())

  assert_vt_is_tensor(operand)
  assert_vt_is_tensor(start_indices)

  offset_dims <- gather_dimension_numbers$offset_dims
  collapsed_slice_dims <- gather_dimension_numbers$collapsed_slice_dims
  operand_batching_dims <- gather_dimension_numbers$operand_batching_dims
  start_indices_batching_dims <- gather_dimension_numbers$start_indices_batching_dims
  start_index_map <- gather_dimension_numbers$start_index_map
  index_vector_dim <- gather_dimension_numbers$index_vector_dim

  operand_shape <- shape(operand)
  operand_rank <- length(operand_shape)
  start_indices_shape <- shape(start_indices)
  start_indices_rank <- length(start_indices_shape)
  slice_sizes_vec <- as.integer(slice_sizes$data)

  any_outside_rank_range <- function(x, rank) {
    any(x < 0L) || any(x >= rank)
  }

  # (C1)
  expected_rank <- length(offset_dims) +
    length(collapsed_slice_dims) +
    length(operand_batching_dims)
  if (operand_rank != expected_rank) {
    # fmt: skip
    cli_abort(c(
      "rank(operand) must equal size(offset_dims) + size(collapsed_slice_dims) + size(operand_batching_dims).", # nolint
      x = "Got rank = {operand_rank}, but expected {expected_rank} (= {length(offset_dims)} + {length(collapsed_slice_dims)} + {length(operand_batching_dims)})." # nolint
    ))
  }

  # (C2)
  if (index_vector_dim < 0L || index_vector_dim > start_indices_rank) {
    error_index_out_of_bounds(
      arg = "index_vector_dim",
      index = index_vector_dim,
      lower = 0L,
      upper = start_indices_rank + 1L
    )
  }

  # (C3)
  expected_start_index_map_size <- if (index_vector_dim < start_indices_rank) {
    start_indices_shape[index_vector_dim + 1L]
  } else {
    1L
  }
  if (length(start_index_map) != expected_start_index_map_size) {
    cli_abort(c(
      "size(start_index_map) must equal the index vector size.",
      x = "Got {length(start_index_map)}, but expected {expected_start_index_map_size}."
    ))
  }

  # (C4)
  if (anyDuplicated(offset_dims)) {
    error_dimension_uniqueness(
      arg = "offset_dims",
      dimensions = offset_dims
    )
  }
  if (is.unsorted(offset_dims)) {
    error_indices_not_sorted(
      arg = "offset_dims",
      indices = offset_dims
    )
  }

  # Compute result rank for C5
  batch_dim_sizes <- if (index_vector_dim == start_indices_rank) {
    start_indices_shape
  } else {
    without(start_indices_shape, index_vector_dim + 1L)
  }
  offset_dim_sizes <- without(
    slice_sizes_vec,
    c(collapsed_slice_dims, operand_batching_dims) + 1L
  )
  result_rank <- length(batch_dim_sizes) + length(offset_dim_sizes)

  # (C5)
  if (any_outside_rank_range(offset_dims, result_rank)) {
    error_index_out_of_bounds(
      arg = "offset_dims",
      index = offset_dims,
      lower = 0L,
      upper = result_rank
    )
  }

  # (C6)
  combined_collapse_batch <- c(collapsed_slice_dims, operand_batching_dims)
  if (anyDuplicated(combined_collapse_batch)) {
    error_dimension_uniqueness(
      arg = "c(collapsed_slice_dims, operand_batching_dims)",
      dimensions = combined_collapse_batch
    )
  }

  # (C7)
  if (is.unsorted(collapsed_slice_dims)) {
    error_indices_not_sorted(
      arg = "collapsed_slice_dims",
      indices = collapsed_slice_dims
    )
  }

  # (C8)
  if (any_outside_rank_range(collapsed_slice_dims, operand_rank)) {
    error_index_out_of_bounds(
      arg = "collapsed_slice_dims",
      index = collapsed_slice_dims,
      lower = 0L,
      upper = operand_rank
    )
  }

  # (C9)
  if (length(collapsed_slice_dims)) {
    collapsed_sizes <- slice_sizes_vec[collapsed_slice_dims + 1L]
    if (any(collapsed_sizes > 1L)) {
      # fmt: skip
      cli_abort(c(
        "slice_sizes[collapsed_slice_dims...] must be <= 1.",
        x = "Got slice_sizes at collapsed_slice_dims: [{paste(collapsed_sizes, collapse = ', ')}]." # nolint
      ))
    }
  }

  # (C10)
  if (is.unsorted(operand_batching_dims)) {
    error_indices_not_sorted(
      arg = "operand_batching_dims",
      indices = operand_batching_dims
    )
  }

  # (C11)
  if (any_outside_rank_range(operand_batching_dims, operand_rank)) {
    error_index_out_of_bounds(
      arg = "operand_batching_dims",
      index = operand_batching_dims,
      lower = 0L,
      upper = operand_rank
    )
  }

  # (C12)
  if (length(operand_batching_dims)) {
    batching_sizes <- slice_sizes_vec[operand_batching_dims + 1L]
    if (any(batching_sizes > 1L)) {
      cli_abort(c(
        "slice_sizes[operand_batching_dims...] must be <= 1.",
        x = "Got slice_sizes at operand_batching_dims: [{paste(batching_sizes, collapse = ', ')}]."
      ))
    }
  }

  # (C13)
  if (anyDuplicated(start_indices_batching_dims)) {
    error_dimension_uniqueness(
      arg = "start_indices_batching_dims",
      dimensions = start_indices_batching_dims
    )
  }

  # (C14)
  if (any_outside_rank_range(start_indices_batching_dims, start_indices_rank)) {
    error_index_out_of_bounds(
      arg = "start_indices_batching_dims",
      index = start_indices_batching_dims,
      lower = 0L,
      upper = start_indices_rank
    )
  }

  # (C15)
  if (test_subset(index_vector_dim, start_indices_batching_dims)) {
    error_index_in_set(
      arg1 = "index_vector_dim",
      arg2 = "start_indices_batching_dims",
      index = index_vector_dim,
      set = start_indices_batching_dims
    )
  }

  # (C16)
  if (length(operand_batching_dims) != length(start_indices_batching_dims)) {
    cli_abort(c(
      "size(operand_batching_dims) must equal size(start_indices_batching_dims).",
      x = "Got {length(operand_batching_dims)} and {length(start_indices_batching_dims)}."
    ))
  }

  # (C17)
  if (length(operand_batching_dims)) {
    batch_shape_operand <- operand_shape[operand_batching_dims + 1L]
    batch_shape_start_indices <- start_indices_shape[
      start_indices_batching_dims + 1L
    ]
    if (!identical(batch_shape_operand, batch_shape_start_indices)) {
      cli_abort(c(
        "Shape of batch dimensions of {.arg operand} and {.arg start_indices} must match.",
        x = "Got {shapevec_repr(batch_shape_operand)} and {shapevec_repr(batch_shape_start_indices)}."
      ))
    }
  }

  # (C18)
  combined_index_batch <- c(start_index_map, operand_batching_dims)
  if (anyDuplicated(combined_index_batch)) {
    error_dimension_uniqueness(
      arg = "c(start_index_map, operand_batching_dims)",
      dimensions = combined_index_batch
    )
  }

  # (C19)
  if (any_outside_rank_range(start_index_map, operand_rank)) {
    error_index_out_of_bounds(
      arg = "start_index_map",
      index = start_index_map,
      lower = 0L,
      upper = operand_rank
    )
  }

  # (C20)
  if (length(slice_sizes_vec) != operand_rank) {
    cli_abort(c(
      "size(slice_sizes) must equal rank(operand).",
      x = "Got {length(slice_sizes_vec)}, but expected {operand_rank}."
    ))
  }

  # (C21)
  if (any(slice_sizes_vec < 0L) || any(slice_sizes_vec > operand_shape)) {
    # fmt: skip
    cli_abort(c(
      "0 <= slice_sizes <= shape(operand).",
      x = "Got slice_sizes = [{paste(slice_sizes_vec, collapse = ', ')}], but operand shape is {shapevec_repr(operand_shape)}." # nolint
    ))
  }

  # (C22)
  batch_dims <- setdiff(xlamisc::seq_len0(result_rank), offset_dims)

  result_shape <- integer(result_rank)
  if (length(batch_dims) > 0L) {
    result_shape[batch_dims + 1L] <- batch_dim_sizes
  }
  if (length(offset_dims) > 0L) {
    result_shape[offset_dims + 1L] <- offset_dim_sizes
  }

  # (C23)
  result_dtype <- operand$type$dtype

  ValueTypes(list(
    ValueType(TensorType(dtype = result_dtype, shape = Shape(result_shape)))
  ))
}

hlo_gather_impl <- hlo_fn(
  OpGather,
  infer_types_gather
)

#' @templateVar mnemonic gather
#' @templateVar not_func_variables gather_dimension_numbers,slice_sizes,indices_are_sorted
#' @template op
#' @param gather_dimension_numbers (`GatherDimensionNumbers`)\cr
#'   The gather dimension numbers.
#' @param slice_sizes (`integer()`)\cr
#'   The sizes of the slices to gather.
#' @param indices_are_sorted (`logical(1)`)\cr
#'   Whether indices are sorted.
#' @export
hlo_gather <- function(
  operand,
  start_indices,
  gather_dimension_numbers,
  slice_sizes,
  indices_are_sorted = FALSE
) {
  assert_class(gather_dimension_numbers, "GatherDimensionNumbers")

  # Convert slice_sizes to constant
  slice_sizes_const <- r_to_constant(
    as.integer(slice_sizes),
    dtype = "i64",
    shape = length(slice_sizes)
  )

  hlo_gather_impl(
    values = list(
      operand = operand,
      start_indices = start_indices
    ),
    custom_attrs = list(gather_dimension_numbers = gather_dimension_numbers),
    attrs = list(
      ConstantAttr(name = "slice_sizes", value = slice_sizes_const),
      BoolAttr(
        name = "indices_are_sorted",
        value = as.logical(indices_are_sorted)
      )
    )
  )
}

#' @export
repr.OpGather <- function(x, toplevel = TRUE, simplify_dense = TRUE, ...) {
  values_repr <- repr(x$inputs$values)
  gather_dim_nums <- x$inputs$custom_attrs$gather_dimension_numbers

  attrs_parts <- character()
  for (attr in x$inputs$attrs) {
    attrs_parts <- c(attrs_parts, repr(attr))
  }
  attrs_str <- paste(attrs_parts, collapse = ", ")

  paste0(
    repr(x$outputs),
    " = ",
    repr(x$name),
    "(",
    values_repr,
    ") {\n",
    "dimension_numbers = ",
    repr(gather_dim_nums),
    ",\n",
    attrs_str,
    "\n}: ",
    repr(x$signature)
  )
}
