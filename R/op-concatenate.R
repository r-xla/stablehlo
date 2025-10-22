#' @include op.R hlo.R
NULL

OpConcatenate <- new_Op("OpConcatenate", "concatenate")

infer_types_concatenate <- function(..., dimension) {
  dots <- list(...)
  input_dims <- lapply(dots, \(x) shape(x))
  dim <- dimension@value@data + 1

  # (C3) 0 < size(inputs)
  if (!length(dots)) {
    cli_abort("must have at least one input")
  }

  lapply(dots, function(x) {
    stopifnot(inherits(x, ValueType))
  })

  # (C1) same(element_type(inputs...))
  dtypes <- lapply(dots, \(x) x@type@dtype)
  if (length(unique(dtypes)) != 1) {
    cli_abort("Each input must have same element type")
  }

  # (C4) 0 <= dimension < rank(inputs[0])
  if (length(shape(dots[[1]])) < dim) {
    cli_abort("dimension must exist in inputs")
  }

  # (C2) same(shape(inputs...)) except for dim(inputs..., dimension)
  dims_ <- lapply(input_dims, \(x) x[-dim])
  if (!all(dims_ == dims_[[1]])) {
    cli_abort("Each input must have same shape (except dimension)")
  }

  result_dims <- input_dims[[1]]
  result_dims[dim] <- sum(vapply(input_dims, \(x) x[dim], integer(1)))

  ValueTypes(list(
    ValueType(
      TensorType(
        dtype = dots[[1]]@type@dtype,
        shape = Shape(result_dims)
      )
    )
  ))
}

hlo_concatenate_impl <- hlo_fn(OpConcatenate, infer_types_concatenate)

#' @title mnemonic concatenate
#' @param ... ([`FuncVariable`])\cr
#' @export
hlo_concatenate <- function(..., dimension) {
  dots <- list(...)
  if (length(dimension) != 1) {
    cli_abort("dimension must be a scalar")
  }
  hlo_concatenate_impl(
    values = dots,
    custom_attrs = list(dimension = as.integer(dimension))
  )
}

method(repr, OpConcatenate) <- function(x) {
  paste0(
    repr(x@outputs),
    " = ",
    repr(x@name),
    " ",
    repr(x@inputs, simplify_dense = TRUE),
    ": ",
    repr(x@signature)
  )
}
