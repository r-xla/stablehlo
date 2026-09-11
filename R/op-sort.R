#' @include op.R hlo.R
NULL

OpSort <- new_Op("OpSort", "sort")

#' @rdname hlo_sort
#' @export
infer_types_sort <- function(..., dimension, is_stable, comparator) {
  assert_vts_are_tensors(...)
  assert_const(dimension, dtype = as_dtype("i64"), shape = integer())
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

  # (C3) Every input has the same shape. Folded with `shape_meet` rather than
  # compared pairwise against the first: "may be equal" is not transitive, so a
  # pairwise check would accept `(3, ?, 4)` because each shape may match the
  # first. The fold refuses that, and it refines as it goes, so the result
  # shape below is the most any input knows.
  # `call` so the error is reported against infer_types_sort() rather than this
  # local helper.
  error_shapes_differ <- function(call = rlang::caller_env()) {
    shapes_str <- vapply(input_dims, shapevec_repr, character(1))
    cli_abort(
      c(
        "Each input must have the same shape",
        x = "Got shapes {shapes_str}."
      ),
      call = call
    )
  }
  # Rank first, with this op's own wording: it is a compile-time constant, so
  # unlike a size it is never deferred, and `shape_meet` would report it in its
  # own words.
  rank <- length(input_dims[[1L]])
  if (!all(lengths(input_dims) == rank)) {
    error_shapes_differ()
  }
  infer_frame <- environment()
  result_dims <- withCallingHandlers(
    shapes_meet(input_dims, arg = "input"),
    ErrorDimSizeMismatch = function(cnd) error_shapes_differ(call = infer_frame)
  )

  # (C4)
  num_dims <- length(result_dims)
  if ((dimension < -num_dims) || (dimension >= num_dims)) {
    error_index_out_of_bounds(
      arg = "dimension",
      index = dimension,
      lower = -num_dims,
      upper = num_dims
    )
  }

  # (C2), (C3) Each output keeps its input's dtype but takes the met shape, so
  # sorting a dynamic input alongside a static one gives static results.
  ValueTypes(lapply(
    dots,
    \(x) ValueType(TensorType(dtype = x$type$dtype, shape = Shape(result_dims)))
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
        dtype = as_dtype("i64")
      ),
      BoolAttr(name = "is_stable", value = as.logical(is_stable))
    ),
    simplify = FALSE
  )
}
