#' @include op.R hlo.R type_inference.R
NULL

OpScatter <- new_Op("OpScatter", "scatter")

#' @title ScatterDimensionNumbers
#' @description
#' Represents the scatter dimension numbers.
#' @param update_window_dims (`integer()`)\cr
#'   The update window dimensions.
#' @param inserted_window_dims (`integer()`)\cr
#'   The inserted window dimensions.
#' @param input_batching_dims (`integer()`)\cr
#'   The input batching dimensions.
#' @param scatter_indices_batching_dims (`integer()`)\cr
#'   The scatter indices batching dimensions.
#' @param scatter_dims_to_operand_dims (`integer()`)\cr
#'   Maps scatter dimensions to operand dimensions.
#' @param index_vector_dim (`integer(1)`)\cr
#'   The index vector dimension.
#' @export
ScatterDimensionNumbers <- function(
  update_window_dims,
  inserted_window_dims,
  input_batching_dims = integer(),
  scatter_indices_batching_dims = integer(),
  scatter_dims_to_operand_dims,
  index_vector_dim
) {
  assert_int(index_vector_dim)
  structure(
    list(
      update_window_dims = as.integer(update_window_dims),
      inserted_window_dims = as.integer(inserted_window_dims),
      input_batching_dims = as.integer(input_batching_dims),
      scatter_indices_batching_dims = as.integer(scatter_indices_batching_dims),
      scatter_dims_to_operand_dims = as.integer(scatter_dims_to_operand_dims),
      index_vector_dim = as.integer(index_vector_dim)
    ),
    class = "ScatterDimensionNumbers"
  )
}

#' @export
repr.ScatterDimensionNumbers <- function(x, ...) {
  parts <- c(
    sprintf(
      "update_window_dims = [%s]",
      paste0(x$update_window_dims, collapse = ", ")
    ),
    sprintf(
      "inserted_window_dims = [%s]",
      paste0(x$inserted_window_dims, collapse = ", ")
    )
  )

  if (
    length(x$input_batching_dims) > 0L ||
      length(x$scatter_indices_batching_dims) > 0L
  ) {
    parts <- c(
      parts,
      sprintf(
        "input_batching_dims = [%s]",
        paste0(x$input_batching_dims, collapse = ", ")
      ),
      sprintf(
        "scatter_indices_batching_dims = [%s]",
        paste0(x$scatter_indices_batching_dims, collapse = ", ")
      )
    )
  }

  parts <- c(
    parts,
    sprintf(
      "scatter_dims_to_operand_dims = [%s]",
      paste0(x$scatter_dims_to_operand_dims, collapse = ", ")
    ),
    sprintf("index_vector_dim = %d", x$index_vector_dim)
  )

  sprintf(
    "#stablehlo.scatter<%s>",
    paste(parts, collapse = ", ")
  )
}

#' @param scatter_dimension_numbers (`ScatterDimensionNumbers`)\cr
#'   The scatter dimension numbers.
#' @param indices_are_sorted (`logical(1)`)\cr
#'   Whether indices are sorted.
#' @param unique_indices (`logical(1)`)\cr
#'   Whether indices are unique.
#' @param update_computation ([`Func`])\cr
#'   The update computation function.
#' @rdname hlo_scatter
#' @export
infer_types_scatter <- function(
  inputs,
  scatter_indices,
  updates,
  scatter_dimension_numbers,
  indices_are_sorted,
  unique_indices,
  update_computation
) {
  inputs <- as.list(inputs)
  updates <- as.list(updates)

  assert_class(scatter_dimension_numbers, "ScatterDimensionNumbers")
  assert_const(indices_are_sorted, dtype = "i1", shape = integer())
  assert_const(unique_indices, dtype = "i1", shape = integer())
  assert_func(update_computation)

  # (C5)
  if (!length(inputs) || !length(updates)) {
    cli_abort("scatter requires at least one input and one update.")
  }

  if (length(inputs) != length(updates)) {
    cli_abort(c(
      "Number of inputs must equal number of updates.",
      x = "Got {length(inputs)} inputs and {length(updates)} updates."
    ))
  }

  lapply(inputs, assert_vt_is_tensor)
  assert_vt_is_tensor(scatter_indices)
  lapply(updates, assert_vt_is_tensor)

  num_inputs <- length(inputs)
  update_window_dims <- scatter_dimension_numbers$update_window_dims
  inserted_window_dims <- scatter_dimension_numbers$inserted_window_dims
  input_batching_dims <- scatter_dimension_numbers$input_batching_dims
  scatter_indices_batching_dims <- scatter_dimension_numbers$scatter_indices_batching_dims
  scatter_dims_to_operand_dims <- scatter_dimension_numbers$scatter_dims_to_operand_dims
  index_vector_dim <- scatter_dimension_numbers$index_vector_dim

  input_shape <- shape(inputs[[1L]])
  input_rank <- length(input_shape)
  scatter_indices_shape <- shape(scatter_indices)
  scatter_indices_rank <- length(scatter_indices_shape)
  updates_shape <- shape(updates[[1L]])
  updates_rank <- length(updates_shape)

  # (C1)
  for (i in seq_along(inputs)[-1L]) {
    if (!identical(shape(inputs[[i]]), input_shape)) {
      cli_abort(c(
        "All inputs must have the same shape.",
        x = "inputs[1] has shape {shapevec_repr(input_shape)}, but inputs[{i}] has shape {shapevec_repr(shape(inputs[[i]]))}."
      ))
    }
  }

  # (C2)
  expected_rank <- length(update_window_dims) +
    length(inserted_window_dims) +
    length(input_batching_dims)
  if (input_rank != expected_rank) {
    cli_abort(c(
      "rank(inputs[0]) must equal size(update_window_dims) + size(inserted_window_dims) + size(input_batching_dims).",
      x = "Got rank = {input_rank}, but expected {expected_rank} (= {length(update_window_dims)} + {length(inserted_window_dims)} + {length(input_batching_dims)})."
    ))
  }

  # (C3)
  for (i in seq_along(updates)[-1L]) {
    if (!identical(shape(updates[[i]]), updates_shape)) {
      shapes_str <- paste(
        vapply(updates, shapevec_repr, character(1)),
        collapse = ", "
      )
      cli_abort(c(
        "All updates must have the same shape.",
        x = "Got shapes: {shapes_str}."
      ))
    }
  }

  # (C6)
  for (i in seq_len(num_inputs)) {
    if (inputs[[i]]$type$dtype != updates[[i]]$type$dtype) {
      error_unequal_types(
        arg1 = "inputs",
        arg2 = "updates",
        index = i - 1L,
        expected = "must have the same element type",
        actual1 = repr(inputs[[i]]$type$dtype),
        actual2 = repr(updates[[i]]$type$dtype)
      )
    }
  }

  # (C7)
  if (anyDuplicated(update_window_dims)) {
    error_dimension_uniqueness(
      arg = "update_window_dims",
      dimensions = update_window_dims
    )
  }
  if (is.unsorted(update_window_dims)) {
    error_indices_not_sorted(
      arg = "update_window_dims",
      indices = update_window_dims
    )
  }

  any_outside_rank_range <- function(x, rank) {
    any(x < 0L) || any(x >= rank)
  }

  update_rank <- ndims(updates[[1L]])
  # (C8)
  if (any_outside_rank_range(update_window_dims, update_rank)) {
    error_index_out_of_bounds(
      arg = "update_window_dims",
      index = update_window_dims,
      lower = 0L,
      upper = update_rank
    )
  }

  # (C9)
  combined_dims <- c(inserted_window_dims, input_batching_dims)
  if (anyDuplicated(combined_dims)) {
    error_dimension_uniqueness(
      arg = "c(inserted_window_dims, input_batching_dims)",
      dimensions = combined_dims
    )
  }

  # (C10)
  if (is.unsorted(inserted_window_dims)) {
    cli_abort(c(
      "inserted_window_dims must be sorted.",
      x = "Got [{paste(inserted_window_dims, collapse = ', ')}]."
    ))
  }

  # (C11)
  if (any_outside_rank_range(inserted_window_dims, input_rank)) {
    error_index_out_of_bounds(
      arg = "inserted_window_dims",
      index = inserted_window_dims,
      lower = 0L,
      upper = input_rank
    )
  }

  # (C12)
  if (is.unsorted(input_batching_dims)) {
    cli_abort(c(
      "input_batching_dims must be sorted.",
      x = "Got [{paste(input_batching_dims, collapse = ', ')}]."
    ))
  }

  # (C13)
  if (any_outside_rank_range(input_batching_dims, input_rank)) {
    error_index_out_of_bounds(
      arg = "input_batching_dims",
      index = input_batching_dims,
      lower = 0L,
      upper = input_rank
    )
  }

  # (C14)
  if (anyDuplicated(scatter_indices_batching_dims)) {
    error_dimension_uniqueness(
      arg = "scatter_indices_batching_dims",
      dimensions = scatter_indices_batching_dims
    )
  }

  # (C15) 0 <= scatter_indices_batching_dims < rank(scatter_indices)
  if (
    any_outside_rank_range(scatter_indices_batching_dims, scatter_indices_rank)
  ) {
    error_index_out_of_bounds(
      arg = "scatter_indices_batching_dims",
      index = scatter_indices_batching_dims,
      lower = 0L,
      upper = scatter_indices_rank
    )
  }

  # (C16)
  if (test_subset(index_vector_dim, scatter_indices_batching_dims)) {
    error_index_in_set(
      arg1 = "index_vector_dim",
      arg2 = "scatter_indices_batching_dims",
      index = index_vector_dim,
      set = scatter_indices_batching_dims
    )
  }

  # (C17)
  if (length(input_batching_dims) != length(scatter_indices_batching_dims)) {
    cli_abort(c(
      "size(input_batching_dims) must equal size(scatter_indices_batching_dims).",
      x = "Got {length(input_batching_dims)} and {length(scatter_indices_batching_dims)}."
    ))
  }

  # (C18)
  batch_shape_inputs <- input_shape[input_batching_dims + 1L]
  batch_shape_scatter <- scatter_indices_shape[
    scatter_indices_batching_dims + 1L
  ]
  if (!identical(batch_shape_inputs, batch_shape_scatter)) {
    cli_abort(
      "Shape of batch dimensions of inputs and scatter_indices must match.",
      x = "Got {shapevec_repr(batch_shape_inputs)} and {shapevec_repr(batch_shape_scatter)}."
    )
  }

  # (C19)
  expected_scatter_dims_size <- if (index_vector_dim < scatter_indices_rank) {
    scatter_indices_shape[index_vector_dim + 1L]
  } else {
    1L
  }
  if (length(scatter_dims_to_operand_dims) != expected_scatter_dims_size) {
    cli_abort(c(
      "size(scatter_dims_to_operand_dims) must equal the index vector size.",
      x = "Got {length(scatter_dims_to_operand_dims)}, but expected {expected_scatter_dims_size}."
    ))
  }

  # (C20)
  combined_operand_dims <- c(scatter_dims_to_operand_dims, input_batching_dims)
  if (anyDuplicated(combined_operand_dims)) {
    error_dimension_uniqueness(
      arg = "c(scatter_dims_to_operand_dims, input_batching_dims)",
      dimensions = combined_operand_dims
    )
  }

  # (C21) 0 <= scatter_dims_to_operand_dims < rank(inputs[0])
  if (any_outside_rank_range(scatter_dims_to_operand_dims, input_rank)) {
    error_index_out_of_bounds(
      arg = "scatter_dims_to_operand_dims",
      index = scatter_dims_to_operand_dims,
      lower = 0L,
      upper = input_rank
    )
  }

  # (C22)
  if (index_vector_dim < 0L || (index_vector_dim > scatter_indices_rank)) {
    error_index_out_of_bounds(
      arg = "index_vector_dim",
      index = index_vector_dim,
      lower = 0L,
      upper = scatter_indices_rank + 1L # if it's equal to scatter_indices_rank, the last dim is implicit
    )
  }

  update_scatter_dims <- setdiff(
    xlamisc::seq_len0(updates_rank),
    update_window_dims
  )

  # From semantics section:
  update_scatter_dim_sizes <- if (index_vector_dim == scatter_indices_rank) {
    scatter_indices_shape
  } else {
    without(scatter_indices_shape, index_vector_dim + 1L)
  }

  update_window_dim_sizes <- without(
    input_shape,
    c(inserted_window_dims, input_batching_dims) + 1L
  )

  expected_updates_shape <- integer(updates_rank)
  if (length(update_scatter_dims) > 0L) {
    expected_updates_shape[update_scatter_dims + 1L] <- update_scatter_dim_sizes
  }
  if (length(update_window_dims) > 0L) {
    expected_updates_shape[update_window_dims + 1L] <- update_window_dim_sizes
  }

  # (C4)
  actual_window_sizes <- updates_shape[update_window_dims + 1L]
  if (any(actual_window_sizes > update_window_dim_sizes)) {
    cli_abort(c(
      "update_window_dim_sizes must not exceed input dimensions.",
      x = "Got update window sizes [{paste(actual_window_sizes, collapse = ', ')}], but max allowed is [{paste(update_window_dim_sizes, collapse = ', ')}]."
    ))
  }

  # Verify the scatter dims match
  if (length(update_scatter_dims) > 0L) {
    actual_scatter_sizes <- updates_shape[update_scatter_dims + 1L]
    if (!identical(actual_scatter_sizes, update_scatter_dim_sizes)) {
      cli_abort(c(
        "Update scatter dimension sizes must match scatter_indices shape (excluding index_vector_dim).",
        x = "Got [{paste(actual_scatter_sizes, collapse = ', ')}], but expected [{paste(update_scatter_dim_sizes, collapse = ', ')}]."
      ))
    }
  }

  # (C23) update_computation has correct type
  body_out_types <- ValueTypes(func_output_types(update_computation))
  if (length(body_out_types) != num_inputs) {
    cli_abort(c(
      "update_computation must return {num_inputs} tensor{?s}.",
      x = "Got {length(body_out_types)} outputs."
    ))
  }

  # Verify output types are scalar tensors with correct dtypes
  for (i in seq_len(num_inputs)) {
    out_type <- body_out_types[[i]]
    if (length(shape(out_type)) != 0L) {
      cli_abort(c(
        "update_computation outputs must be 0-D tensors.",
        x = "Output {i} has shape {shapevec_repr(shape(out_type))}."
      ))
    }
  }

  # (C24) shape(inputs...) = shape(results...)
  # (C25) element_type(results[i]) = Ei for all i in [0,N)
  # The output element types come from the update_computation outputs
  result_types <- lapply(seq_len(num_inputs), function(i) {
    out_dtype <- body_out_types[[i]]$type$dtype
    ValueType(TensorType(dtype = out_dtype, shape = Shape(input_shape)))
  })

  ValueTypes(result_types)
}
hlo_scatter_impl <- hlo_fn(
  OpScatter,
  infer_types_scatter,
  value_list_names = c("inputs", "updates")
)

#' @templateVar mnemonic scatter
#' @templateVar not_func_variables scatter_dimension_numbers,indices_are_sorted,unique_indices,update_computation
#' @template op
#' @export
hlo_scatter <- function(
  inputs,
  scatter_indices,
  updates,
  scatter_dimension_numbers,
  indices_are_sorted = FALSE,
  unique_indices = FALSE,
  update_computation
) {
  inputs <- ensure_func_vals(inputs)
  updates <- ensure_func_vals(updates)

  assert_class(scatter_dimension_numbers, "ScatterDimensionNumbers")

  hlo_scatter_impl(
    values = list(
      inputs = inputs,
      scatter_indices = scatter_indices,
      updates = updates
    ),
    funcs = list(update_computation = update_computation),
    custom_attrs = list(scatter_dimension_numbers = scatter_dimension_numbers),
    attrs = list(
      BoolAttr(
        name = "indices_are_sorted",
        value = as.logical(indices_are_sorted)
      ),
      BoolAttr(name = "unique_indices", value = as.logical(unique_indices))
    ),
  )
}

#' @export
repr.OpScatter <- function(x, toplevel = TRUE, simplify_dense = TRUE, ...) {
  values_repr <- repr(x$inputs$values)
  scatter_dim_nums <- x$inputs$custom_attrs$scatter_dimension_numbers

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
    ")",
    repr(x$inputs$funcs),
    " {\n",
    "scatter_dimension_numbers = ",
    repr(scatter_dim_nums),
    ",\n",
    attrs_str,
    "\n}: ",
    repr(x$signature)
  )
}
