#' @include op.R hlo.R
NULL

OpConcatenate <- new_Op("OpConcatenate", "concatenate")

#' @rdname hlo_concatenate
#' @export
infer_types_concatenate <- function(..., dimension) {
  assert_vts_are_tensors(...)
  dots <- list(...)
  input_dims <- lapply(dots, \(x) shape(x))

  # (C3) 0 < size(inputs)
  if (!length(dots)) {
    cli_abort("must have at least one input")
  }

  # (C1) same(element_type(inputs...))
  dtypes <- lapply(dots, \(x) x$type$dtype)
  if (length(unique(dtypes)) != 1) {
    cli_abort("Each input must have same element type")
  }

  # Convert 0-based dimension to 1-based for R indexing
  dim_r <- dimension + 1L

  # (C4) 0 <= dimension < rank(inputs[0])
  num_dims <- length(shape(dots[[1]]))
  if (dimension < 0L || dimension >= num_dims) {
    error_dimension_out_of_range(
      arg = "dimension",
      dimension = dimension,
      ndims = num_dims
    )
  }

  # (C2) same(shape(inputs...)) except for dim(inputs..., dimension)
  dims_ <- lapply(input_dims, \(x) x[-dim_r])
  if (!all(dims_ == dims_[[1]])) {
    cli_abort("Each input must have same shape (except dimension)")
  }

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
