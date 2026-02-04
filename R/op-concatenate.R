#' @include op.R hlo.R conditions.R
NULL

OpConcatenate <- new_Op("OpConcatenate", "concatenate")

#' @title ErrorConcatenateShapes
#' @description Error when input shapes don't match (except at given dimensions)
#' @param dimensions (`integer()`)\cr The dimensions where shapes may differ (0-based)
#' @param shapes (`list()` of `Shape`)\cr The input shapes
#' @param call (`call` or `NULL`)\cr Call that generated the error
#' @param class (`character()`)\cr Additional classes to prepend
#' @param signal (`logical(1)`)\cr Whether to signal the error (default TRUE)
#' @export
error_concatenate_shapes <- function(
  dimensions,
  shapes,
  call = sys.call(-1)[1L],
  class = character(),
  signal = TRUE
) {
  error_stablehlo(
    dimensions = as.integer(dimensions),
    shapes = shapes,
    call = call,
    class = c(class, "ErrorConcatenateShapes"),
    signal = signal
  )
}

#' @export
conditionMessage.ErrorConcatenateShapes <- function(c, ...) {
  # nolint next
  shapes <- vapply(c$shapes, repr, character(1))
  dims_str <- paste0(c$dimensions, collapse = ", ") # nolint
  format_error(
    c(
      "All inputs must have the same shape, except in dimension(s) {dims_str}.",
      x = "Got shapes {shapes}."
    ),
    .envir = environment()
  )
}

#' @export
to_one_based.ErrorConcatenateShapes <- function(x, ...) {
  x$dimensions <- x$dimensions + 1L
  x
}

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
    # nolint next
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
    error_concatenate_shapes(
      dimensions = dimension,
      shapes = lapply(input_dims, Shape)
    )
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
