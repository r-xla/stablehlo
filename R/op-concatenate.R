#' @include op.R hlo.R
NULL

OpConcatenate <- new_Op("OpConcatenate", "concatenate")

infer_types_concatenate <- function(..., dimension) {
  dots <- list(...)
  input_dims <- lapply(dots, \(x) shape(x))
  dim <- dimension@value@data + 1

  lapply(dots, function(x) {
    stopifnot(inherits(x, ValueType))
  })

  # (C1) same(element_type(inputs...))
  dtypes <- lapply(dots, \(x) x@type@dtype)
  if (!all(vapply(dtypes, \(x) x == dtypes[[1]], logical(1)))) {
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

  # (C3) 0 < size(inputs)
  if (!length(dots)) {
    cli_abort("must have at least one input")
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
  dim_attr <- hlo_tensor(
    as.integer(dimension),
    dtype = "i64",
    func = Func()
  )
  hlo_concatenate_impl(
    values = dots,
    attrs = list(dimension = dim_attr)
  )
}

method(repr, OpConcatenate) <- function(x) {
  paste0(
    repr(x@outputs),
    " = ",
    repr(x@name),
    " ",
    repr(x@inputs, simplify_dense = TRUE, simplify_attrs = TRUE),
    ": ",
    repr(x@signature)
  )
}
