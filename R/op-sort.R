#' @include op.R hlo.R
NULL

OpSort <- new_Op("OpSort", "sort")

infer_types_sort <- function(..., dimension, is_stable, comparator) {
  dots <- list(...)
  input_dims <- lapply(dots, \(x) shape(x))
  # dimension <- dimension@value@data + 1

  lapply(dots, function(x) {
    stopifnot(inherits(x, ValueType))
  })

  # (C1) 0 < size(inputs).
  if (!length(dots)) {
    cli_abort("provide at least one input")
  }

  # (C3) same(shape(inputs...) + shape(results...)).
  if (
    !all(vapply(input_dims, \(x) identical(input_dims[[1]], x), logical(1)))
  ) {
    cli_abort("each input must have the same shape")
  }

  # (C4) -R <= dimension < R, where R = rank(inputs[0]).
  if (dimension > length(input_dims[[1]])) {
    cli_abort("dimension must be present in inputs")
  }
  if (dimension < 1) {
    cli_abort("dimension must be present in inputs")
  }

  # (C2) type(inputs...) = type(results...).
  ValueTypes(lapply(
    dots,
    \(x) ValueType(TensorType(dtype = x@type@dtype, shape = Shape(shape(x))))
  ))
}

hlo_sort_impl <- hlo_fn(OpSort, infer_types_sort)


#' @templateVar mnemonic sort
#' @template op
#' @export
hlo_sort <- function(..., dimension, is_stable, comparator) {
  dots <- list(...)
  hlo_sort_impl(
    values = dots,
    funcs = list(comparator = comparator),
    custom_attrs = list(
      dimension = as.integer(dimension),
      is_stable = as.logical(is_stable)
    )
  )
}

method(repr, OpSort) <- function(x) {
  paste0(
    repr(x@outputs),
    " = ",
    repr(x@name),
    " ",
    repr(x@inputs, simplify_dense = TRUE),
    sprintf(
      " {\ndimension = %d :i64,\nis_stable = %s\n} ",
      x@inputs@custom_attrs$dimension - 1,
      tolower(as.character(x@inputs@custom_attrs$is_stable))
    ),
    ": ",
    repr(x@signature)
  )
}
