#' @include op.R hlo.R
NULL

OpSort <- new_Op("OpSort", "sort")

#' @rdname hlo_sort
#' @export
infer_types_sort <- function(..., dimension, is_stable, comparator) {
  assert_vts_are_tensors(...)
  dots <- list(...)
  input_dims <- lapply(dots, \(x) shape(x))

  # (C1) 0 < size(inputs).
  if (!length(dots)) {
    cli_abort("provide at least one input")
  }

  # (C3) same(shape(inputs...) + shape(results...)).
  if (
    !all(vapply(input_dims[-1], \(x) identical(input_dims[[1]], x), logical(1)))
  ) {
    cli_abort("each input must have the same shape")
  }

  # (C4) 0 <= dimension < R, where R = rank(inputs[0]).
  num_dims <- length(input_dims[[1]])
  if (dimension < 0L || dimension >= num_dims) {
    dimension_out_of_range_error(
      arg = "dimension",
      dimension = dimension,
      ndims = num_dims
    )
  }

  # (C2) type(inputs...) = type(results...).
  ValueTypes(lapply(
    dots,
    \(x) ValueType(TensorType(dtype = x$type$dtype, shape = Shape(shape(x))))
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
    attrs = list(
      ScalarAttr(
        name = "dimension",
        value = as.integer(dimension),
        dtype = IntegerType(64L)
      ),
      BoolAttr(name = "is_stable", value = as.logical(is_stable))
    )
  )
}
