#' @include op.R hlo.R
NULL

OpSort <- new_Op("OpSort", "sort")

#' @rdname hlo_sort
#' @export
infer_types_sort <- function(..., dimension, is_stable, comparator) {
  assert_vts_are_tensors(...)
  assert_const(dimension, dtype = IntegerType(64L), shape = integer())
  assert_const(is_stable, dtype = "i1", shape = integer())
  assert_func(comparator)
  dimension <- dimension$data
  is_stable <- is_stable$data

  dots <- list(...)
  input_dims <- lapply(dots, \(x) shape(x))

  # (C1)
  if (!length(dots)) {
    cli_abort("provide at least one input")
  }

  # (C3)
  if (
    !all(vapply(input_dims[-1], \(x) identical(input_dims[[1]], x), logical(1)))
  ) {
    # nolint next
    shapes_str <- vapply(input_dims, shapevec_repr, character(1))
    cli_abort(c(
      "Each input must have the same shape",
      x = "Got shapes {shapes_str}."
    ))
  }

  # (C4)
  num_dims <- length(input_dims[[1]])
  if ((dimension < -num_dims) || (dimension >= num_dims)) {
    error_index_out_of_bounds(
      arg = "dimension",
      index = dimension,
      lower = -num_dims,
      upper = num_dims
    )
  }

  # (C2), (C3)
  ValueTypes(lapply(
    dots,
    \(x) ValueType(x$type)
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
