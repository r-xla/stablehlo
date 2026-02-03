#' @include op.R hlo.R
NULL

OpConcatenate <- new_Op("OpConcatenate", "concatenate")

#' @rdname hlo_concatenate
#' @export
infer_types_concatenate <- function(..., dimension) {
  assert_vts_are_tensors(...)
  assert_const(dimension, dtype = IntegerType(64L), shape = integer())
  dimension <- dimension$data

  dots <- list(...)
  input_dims <- lapply(dots, \(x) shape(x))

  # (C3)
  if (!length(dots)) {
    cli_abort("must have at least one input")
  }

  # (C1)
  dtypes <- lapply(dots, \(x) x$type$dtype)
  if (length(unique(dtypes)) != 1) {
    dtype_reprs <- vapply(dtypes, repr, character(1))
    cli_abort(c(
      "Each input must have same data type",
      x = "Got {dtype_reprs}."
    ))
  }

  # Convert 0-based dimension to 1-based for R indexing
  dim_r <- dimension + 1L

  # (C4)
  num_dims <- length(shape(dots[[1]]))
  if (dimension >= num_dims) {
    error_index_out_of_bounds(
      arg = "dimension",
      index = dimension,
      lower = 0L,
      upper = num_dims
    )
  }

  # (C2)
  dims_no_concat <- lapply(input_dims, \(x) x[-dim_r])
  if (
    !all(vapply(dims_no_concat, identical, logical(1), dims_no_concat[[1]]))
  ) {
    # fmt: skip
    shapes_str <- paste0( # nolint
      vapply(input_dims, shapevec_repr, character(1)),
      collapse = ", "
    )
    cli_abort(c(
      "Each input must have same shape (except for the concatenated dimension)",
      x = "Got {shapes_str} for dimension {dimension}."
    ))
  }

  # (C6)
  result_dims <- input_dims[[1]]
  result_dims[dim_r] <- sum(vapply(
    input_dims,
    \(x) x[dim_r],
    integer(1)
  ))

  ValueTypes(list(
    ValueType(
      TensorType(
        dtype = dots[[1]]$type$dtype,
        shape = Shape(result_dims)
      )
    )
  ))
}

hlo_concatenate_impl <- hlo_fn(OpConcatenate, infer_types_concatenate)

#' @templateVar mnemonic concatenate
#' @template op
#' @export
hlo_concatenate <- function(..., dimension) {
  dots <- list(...)
  if (length(dimension) != 1) {
    cli_abort("dimension must be a scalar")
  }
  hlo_concatenate_impl(
    values = dots,
    attrs = list(
      ScalarAttr(
        name = "dimension",
        value = as.integer(dimension),
        dtype = IntegerType(64L)
      )
    )
  )
}
