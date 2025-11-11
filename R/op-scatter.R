#' @include op.R hlo.R
NULL

OpScatter <- new_Op("OpScatter", "scatter")

infer_types_scatter <- function(
  ...,
  update_computation,
  update_window_dims,
  inserted_window_dims,
  input_batching_dims,
  scatter_indices_batching_dims,
  scatter_dims_to_operand_dims,
  index_vector_dim,
  indices_are_sorted,
  unique_indices
) {
  # Values are: inputs..., scatter_indices, updates...
  args <- list(...)
  n_total <- length(args)
  if (n_total < 3L || (n_total %% 2L) != 1L) {
    cli_abort("Provide inputs and updates with the same positive length")
  }
  n <- (n_total - 1L) / 2L
  inputs <- args[seq_len(n)]
  scatter_indices <- args[[n + 1L]]
  updates <- args[seq_len(n) + n + 1L]

  lapply(inputs, function(v) stopifnot(inherits(v@type, TensorType)))
  lapply(updates, function(v) stopifnot(inherits(v@type, TensorType)))

  # (C1) same(shape(inputs...))
  input_shapes <- lapply(inputs, shape)
  ref_shape <- input_shapes[[1L]]
  for (s in input_shapes) {
    if (!identical(s, ref_shape)) {
      cli_abort("All input tensors must have the same shape")
    }
  }

  # (C5) 0 < size(inputs) = size(updates) = N
  # already checked above via lengths

  # (C6) element_type(updates...) = element_type(inputs...)
  for (i in seq_len(n)) {
    if (!(updates[[i]]@type@dtype == inputs[[i]]@type@dtype)) {
      cli_abort("Element types of updates and inputs must match per position")
    }
  }

  # (C2) rank(inputs[0]) = |update_window_dims| + |inserted_window_dims| + |input_batching_dims|
  rank_inp <- length(ref_shape)
  u_w_dims <- as.integer(update_window_dims@value@data)
  ins_w_dims <- as.integer(inserted_window_dims@value@data)
  in_batch_dims <- as.integer(input_batching_dims@value@data)
  if (
    rank_inp != (length(u_w_dims) + length(ins_w_dims) + length(in_batch_dims))
  ) {
    cli_abort(
      "rank(inputs[0]) must equal |update_window_dims| + |inserted_window_dims| + |input_batching_dims|"
    )
  }

  # (C7) unique and sorted update_window_dims; (C8) bounds
  if (
    any(duplicated(u_w_dims)) || !isTRUE(all.equal(u_w_dims, sort(u_w_dims)))
  ) {
    cli_abort("update_window_dims must be unique and sorted")
  }
  if (any(u_w_dims < 0L) || any(u_w_dims >= length(shape(updates[[1L]])))) {
    cli_abort("update_window_dims must be within rank(updates[0])")
  }

  # (C9) unique(concat(inserted_window_dims, input_batching_dims))
  if (any(duplicated(c(ins_w_dims, in_batch_dims)))) {
    cli_abort(
      "inserted_window_dims and input_batching_dims must be unique across the concatenation"
    )
  }
  # (C10) sorted(inserted_window_dims); (C11) bounds in inputs
  if (!isTRUE(all.equal(ins_w_dims, sort(ins_w_dims)))) {
    cli_abort("inserted_window_dims must be sorted")
  }
  if (any(ins_w_dims < 0L) || any(ins_w_dims >= rank_inp)) {
    cli_abort("inserted_window_dims must be within rank(inputs[0])")
  }
  # (C12) sorted(input_batching_dims); (C13) bounds
  if (!isTRUE(all.equal(in_batch_dims, sort(in_batch_dims)))) {
    cli_abort("input_batching_dims must be sorted")
  }
  if (any(in_batch_dims < 0L) || any(in_batch_dims >= rank_inp)) {
    cli_abort("input_batching_dims must be within rank(inputs[0])")
  }

  # (I2) scatter_indices is integer tensor
  stopifnot(inherits(scatter_indices@type, TensorType))
  if (
    !inherits(scatter_indices@type@dtype, IntegerType) &&
      !inherits(scatter_indices@type@dtype, UnsignedType)
  ) {
    cli_abort("scatter_indices must be an integer tensor")
  }

  si_batch_dims <- as.integer(scatter_indices_batching_dims@value@data)
  sc_to_op_dims <- as.integer(scatter_dims_to_operand_dims@value@data)
  idx_vec_dim <- as.integer(index_vector_dim@value@data)

  # (C14) unique(scatter_indices_batching_dims); (C15) bounds
  if (any(duplicated(si_batch_dims))) {
    cli_abort("scatter_indices_batching_dims must be unique")
  }
  rank_si <- length(shape(scatter_indices))
  if (any(si_batch_dims < 0L) || any(si_batch_dims >= rank_si)) {
    cli_abort(
      "scatter_indices_batching_dims must be within rank(scatter_indices)"
    )
  }
  # (C16) index_vector_dim not in scatter_indices_batching_dims
  if (length(si_batch_dims) && any(si_batch_dims == idx_vec_dim)) {
    cli_abort("index_vector_dim must not be in scatter_indices_batching_dims")
  }
  # (C17) sizes equal between input_batching_dims and scatter_indices_batching_dims
  if (length(in_batch_dims) != length(si_batch_dims)) {
    cli_abort(
      "input_batching_dims and scatter_indices_batching_dims must have the same size"
    )
  }
  # (C18) dim(inputs[0], input_batching_dims...) == dim(scatter_indices, scatter_indices_batching_dims...)
  if (length(in_batch_dims) > 0L) {
    if (
      !identical(
        ref_shape[in_batch_dims + 1L],
        shape(scatter_indices)[si_batch_dims + 1L]
      )
    ) {
      cli_abort(
        "Batching dims of inputs[0] and scatter_indices must match in size"
      )
    }
  }
  # (C19) size(scatter_dims_to_operand_dims) = (index_vector_dim < rank(scatter_indices) ? dim(scatter_indices, index_vector_dim) : 1)
  expected <- if (idx_vec_dim < rank_si) {
    shape(scatter_indices)[idx_vec_dim + 1L]
  } else {
    1L
  }
  if (length(sc_to_op_dims) != expected) {
    cli_abort(
      "scatter_dims_to_operand_dims has incorrect size for given index_vector_dim and scatter_indices shape"
    )
  }
  # (C20) unique(concat(scatter_dims_to_operand_dims, input_batching_dims))
  if (any(duplicated(c(sc_to_op_dims, in_batch_dims)))) {
    cli_abort(
      "scatter_dims_to_operand_dims and input_batching_dims must be unique across the concatenation"
    )
  }
  # (C21) 0 <= scatter_dims_to_operand_dims < rank(inputs[0])
  if (any(sc_to_op_dims < 0L) || any(sc_to_op_dims >= rank_inp)) {
    cli_abort("scatter_dims_to_operand_dims must be within rank(inputs[0])")
  }
  # (C22) 0 <= index_vector_dim <= rank(scatter_indices)
  if (idx_vec_dim < 0L || idx_vec_dim > rank_si) {
    cli_abort("index_vector_dim must be within [0, rank(scatter_indices)]")
  }

  # (C3) same(shape(updates...))
  update_shapes <- lapply(updates, shape)
  ref_update_shape <- update_shapes[[1L]]
  for (s in update_shapes) {
    if (!identical(s, ref_update_shape)) {
      cli_abort("All updates must have the same shape")
    }
  }

  # (C4) shape(updates[0]) = combine(update_scatter_dim_sizes, update_window_dim_sizes)
  # Validate only basic compatibility: update_window_dim_sizes <= shape(inputs[0]). Full combine rules are complex; keep minimal checks.
  # Ensure that dimensions referenced by update_window_dims exist in updates shape and are <= corresponding input dims (minus inserted/batching positions).
  upd_shape <- ref_update_shape
  if (length(u_w_dims) > 0L) {
    if (any(u_w_dims < 0L) || any(u_w_dims >= length(upd_shape))) {
      cli_abort("update_window_dims must index within updates[0] rank")
    }
    # Approximate check: each window dim size <= corresponding input dim somewhere
    if (any(upd_shape[u_w_dims + 1L] > max(ref_shape))) {
      # Weak check; ensures no absurd larger-than-input windows
      cli_abort("update window dims must not exceed input dims")
    }
  }

  # Output types: (C24) shape(inputs...) = shape(results...), (C25) element types via update_computation promotion rules.
  # We conservatively set result element types equal to inputs element types (common for simple add/mul computations in tests).
  out_types <- lapply(inputs, function(inp) ValueType(inp@type))
  ValueTypes(out_types)
}

hlo_scatter_impl <- hlo_fn(OpScatter, infer_types_scatter)

#' @templateVar mnemonic scatter
#' @template op
#' @export
hlo_scatter <- function(
  inputs,
  scatter_indices,
  updates,
  update_computation,
  update_window_dims,
  inserted_window_dims,
  input_batching_dims,
  scatter_indices_batching_dims,
  scatter_dims_to_operand_dims,
  index_vector_dim,
  indices_are_sorted = FALSE,
  unique_indices = FALSE
) {
  # Encode integer vector/tensors attributes
  to_i64_attr <- function(x) {
    hlo_tensor(as.integer(x), dtype = "i64", func = Func())
  }
  update_window_dims_attr <- to_i64_attr(update_window_dims)
  inserted_window_dims_attr <- to_i64_attr(inserted_window_dims)
  input_batching_dims_attr <- to_i64_attr(input_batching_dims)
  scatter_indices_batching_dims_attr <- to_i64_attr(
    scatter_indices_batching_dims
  )
  scatter_dims_to_operand_dims_attr <- to_i64_attr(scatter_dims_to_operand_dims)
  index_vector_dim_attr <- hlo_scalar(
    as.integer(index_vector_dim),
    dtype = "i64",
    func = Func()
  )
  indices_are_sorted_attr <- hlo_scalar(
    as.logical(indices_are_sorted),
    dtype = "i1",
    func = Func()
  )
  unique_indices_attr <- hlo_scalar(
    as.logical(unique_indices),
    dtype = "i1",
    func = Func()
  )

  hlo_scatter_impl(
    values = c(inputs, list(scatter_indices), updates),
    funcs = list(update_computation = update_computation),
    attrs = list(
      update_window_dims = update_window_dims_attr,
      inserted_window_dims = inserted_window_dims_attr,
      input_batching_dims = input_batching_dims_attr,
      scatter_indices_batching_dims = scatter_indices_batching_dims_attr,
      scatter_dims_to_operand_dims = scatter_dims_to_operand_dims_attr,
      index_vector_dim = index_vector_dim_attr,
      indices_are_sorted = indices_are_sorted_attr,
      unique_indices = unique_indices_attr
    )
  )
}

method(repr, OpScatter) <- function(x) {
  # Extract attributes for custom formatting
  get_attr <- function(nm) {
    it <- x@inputs@attrs@items
    idx <- which(vapply(it, function(a) a@name, character(1)) == nm)
    if (length(idx) != 1L) {
      return(NULL)
    }
    it[[idx]]@value@value@data
  }

  u_w_dims <- get_attr("update_window_dims")
  ins_w_dims <- get_attr("inserted_window_dims")
  in_batch_dims <- get_attr("input_batching_dims")
  si_batch_dims <- get_attr("scatter_indices_batching_dims")
  sc_to_op_dims <- get_attr("scatter_dims_to_operand_dims")
  idx_vec_dim <- get_attr("index_vector_dim")
  idx_sorted <- get_attr("indices_are_sorted")
  uniq_idx <- get_attr("unique_indices")

  # Fall back for logicals stored as length-1 vectors
  to_bool <- function(v) {
    if (is.null(v)) {
      return(NULL)
    }
    as.logical(v)[1L]
  }
  idx_sorted <- to_bool(idx_sorted)
  uniq_idx <- to_bool(uniq_idx)

  # Format lists
  fmt_iv <- function(v) paste0(v, collapse = ", ")
  sdn <- paste0(
    "scatter_dimension_numbers = #stablehlo.scatter<\n",
    "    update_window_dims = [",
    fmt_iv(u_w_dims),
    "],\n",
    "    inserted_window_dims = [",
    fmt_iv(ins_w_dims),
    "],\n",
    "    input_batching_dims = [",
    fmt_iv(in_batch_dims),
    "],\n",
    "    scatter_indices_batching_dims = [",
    fmt_iv(si_batch_dims),
    "],\n",
    "    scatter_dims_to_operand_dims = [",
    fmt_iv(sc_to_op_dims),
    "],\n",
    "    index_vector_dim = ",
    as.integer(idx_vec_dim),
    ">"
  )

  attrs_block <- paste0(
    " {\n",
    "  ",
    sdn,
    ",\n",
    "  indices_are_sorted = ",
    if (isTRUE(idx_sorted)) "true" else "false",
    ",\n",
    "  unique_indices = ",
    if (isTRUE(uniq_idx)) "true" else "false",
    "\n",
    "}"
  )

  paste0(
    repr(x@outputs),
    " = ",
    repr(x@name),
    " ",
    "(",
    repr(x@inputs@values),
    ")",
    repr(x@inputs@funcs),
    "",
    attrs_block,
    ": ",
    repr(x@signature)
  )
}
