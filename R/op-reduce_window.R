#' @include op.R hlo.R type_inference.R
NULL

OpReduceWindow <- new_Op("OpReduceWindow", "reduce_window")

#' @param ... (Inputs, Init values)
#' @rdname hlo_reduce_window
#' @export
infer_types_reduce_window <- function(
  ...,
  body,
  window_dimensions,
  window_strides,
  base_dilations,
  window_dilations,
  padding
) {
  assert_func(body)
  assert_vts_are_tensors(...)
  assert_const(window_dimensions, dtype = IntegerType(64L), ndims = 1L)
  assert_const(window_strides, dtype = IntegerType(64L), ndims = 1)
  assert_const(base_dilations, dtype = IntegerType(64L), ndims = 1L)
  assert_const(window_dilations, dtype = IntegerType(64L), ndims = 1L)
  assert_const(padding, dtype = IntegerType(64L), ndims = 2L)

  value_types <- list(...)

  # (C1)
  if (length(value_types) %% 2L != 0L) {
    cli_abort(c(
      "Number of arguments must be divisible by 2 (pairs of inputs and init values).",
      i = "Got {length(value_types)} argument{?s}."
    ))
  }
  if (length(value_types) == 0L) {
    cli_abort(c(
      "At least one input and one init value must be provided.",
      i = "Got 0 arguments."
    ))
  }

  num_inputs <- length(value_types) / 2L

  input_value_types <- value_types[seq_len(num_inputs)]
  init_value_types <- value_types[seq_len(num_inputs) + num_inputs]

  # Check init_values are 0-D tensors

  lapply(seq_along(init_value_types), function(i) {
    vt <- init_value_types[[i]]
    if (length(vt$type$shape$dims) != 0L) {
      error_unexpected_type(
        arg = "init_values",
        index = i - 1L, # 0-based
        expected = "must be 0-D tensors",
        actual = paste("shape", shapevec_repr(vt$type$shape$dims))
      )
    }
  })

  # (C2)
  input_shapes <- lapply(input_value_types, function(vt) shape(vt))
  ref_shape <- input_shapes[[1L]]
  if (
    !all(vapply(input_shapes, function(s) identical(s, ref_shape), logical(1L)))
  ) {
    # fmt: skip
    shape_strs <- paste( # nolint
      vapply(input_shapes, shapevec_repr, character(1)),
      collapse = ", "
    )
    cli_abort(c(
      "All inputs to reduce_window must have the same shape.",
      i = "Got shapes: {shape_strs}."
    ))
  }

  # (C3) Each input must have the same dtype as its corresponding init_value
  for (i in seq_len(num_inputs)) {
    input_dtype <- input_value_types[[i]]$type$dtype
    init_dtype <- init_value_types[[i]]$type$dtype
    if (input_dtype != init_dtype) {
      error_unequal_types(
        arg1 = "inputs",
        arg2 = "init_values",
        index = i - 1L, # 0-based
        expected = "must have the same dtype",
        actual1 = repr(input_dtype),
        actual2 = repr(init_dtype)
      )
    }
  }

  rank <- length(ref_shape)
  window_dims <- as.integer(window_dimensions$data)

  # (C4)
  if (length(window_dims) != rank) {
    cli_abort(c(
      "window_dimensions must have length equal to input rank.",
      i = "Expected length {rank}, got {length(window_dims)}."
    ))
  }

  # (C5)
  if (any(window_dims < 1L)) {
    cli_abort(c(
      "window_dimensions must be positive.",
      i = "Got {window_dims}"
    ))
  }

  strides <- as.integer(window_strides$data)
  base_dil <- as.integer(base_dilations$data)
  window_dil <- as.integer(window_dilations$data)
  pad <- padding$data

  # (C12)
  if (nrow(pad) != rank || ncol(pad) != 2L) {
    cli_abort(c(
      "padding must have shape [rank, 2].",
      i = "Expected shape [{rank}, 2], got [{nrow(pad)}, {ncol(pad)}]."
    ))
  }

  # (C6)
  if (length(strides) != rank) {
    cli_abort(c(
      "window_strides must have length equal to input rank.",
      i = "Expected length {rank}, got {length(strides)}."
    ))
  }

  # (C7)
  if (any(strides < 1L)) {
    cli_abort(c(
      "window_strides must be positive.",
      i = "Got {strides}"
    ))
  }

  # (C8)
  if (length(base_dil) != rank) {
    cli_abort(c(
      "base_dilations must have length equal to input rank.",
      i = "Expected length {rank}, got {length(base_dil)}."
    ))
  }
  # (C10)
  if (length(window_dil) != rank) {
    cli_abort(c(
      "window_dilations must have length equal to input rank.",
      i = "Expected length {rank}, got {length(window_dil)}."
    ))
  }

  # (C9)
  if (any(base_dil <= 0)) {
    cli_abort(c(
      "base_dilations must be positive.",
      i = "Got {base_dil}"
    ))
  }
  # (C11)
  if (any(window_dil < 0)) {
    cli_abort(c(
      "window_dilations must be positive.",
      i = "Got {window_dil}"
    ))
  }

  dilated_input <- ifelse(ref_shape == 0L, 0L, (ref_shape - 1L) * base_dil + 1L)
  padded_input <- pad[, 1L] + dilated_input + pad[, 2L]
  dilated_window <- (window_dims - 1L) * window_dil + 1L
  is_empty_window <- padded_input == 0L | dilated_window > padded_input
  result_shape <- ifelse(
    is_empty_window,
    0L,
    as.integer(floor((padded_input - dilated_window) / strides) + 1L)
  )

  body_out_types <- func_output_types(body)
  if (length(body_out_types) != num_inputs) {
    cli_abort(c(
      "Body must return one tensor per input.",
      i = "Expected {num_inputs} output{?s}, got {length(body_out_types)}."
    ))
  }

  out_vts <- lapply(seq_len(num_inputs), function(i) {
    out_elem_vt <- body_out_types[[i]]
    if (length(out_elem_vt$type$shape$dims) != 0L) {
      error_unexpected_type(
        arg = "body output",
        index = i - 1L, # 0-based
        expected = "must be 0-D tensors",
        actual = paste("shape", shapevec_repr(out_elem_vt$type$shape$dims))
      )
    }
    ValueType(
      TensorType(
        dtype = out_elem_vt$type$dtype,
        shape = Shape(result_shape)
      )
    )
  })

  ValueTypes(out_vts)
}

hlo_reduce_window_impl <- hlo_fn(OpReduceWindow, infer_types_reduce_window)

#' @title ReduceWindow Operator
#' @description
#' See \url{https://openxla.org/stablehlo/spec#reduce_window} for details.
#' @param inputs (`list()` of [`FuncValue`])\cr
#'   The input tensor(s) to apply the reduction to.
#' @param init_values (`list()` of [`FuncValue`])\cr
#'   The initial value(s) for the reduction. Must be 0-D tensors.
#' @param window_dimensions (`integer()`)\cr
#'   The size of the window in each dimension.
#' @param window_strides (`integer()`)\cr
#'   The stride of the window in each dimension.
#' @param base_dilations (`integer()`)\cr
#'   The dilation factor for the input tensor.
#' @param window_dilations (`integer()`)\cr
#'   The dilation factor for the window.
#' @param padding (`matrix`)\cr
#'   A matrix with shape `[rank, 2]` specifying the padding before and after
#'   each dimension.
#' @param body (`Func`)\cr
#'   The reduction function to apply to each window.
#' @export
hlo_reduce_window <- function(
  inputs,
  init_values,
  window_dimensions,
  window_strides,
  base_dilations,
  window_dilations,
  padding,
  body
) {
  # Normalize inputs and init_values to lists
  if (!is.list(inputs)) {
    inputs <- list(inputs)
  }
  if (!is.list(init_values)) {
    init_values <- list(init_values)
  }

  attrs <- list(
    constant_attr(
      "window_dimensions",
      as.integer(window_dimensions),
      dtype = "i64",
      shape = c()
    ),
    constant_attr(
      "window_strides",
      as.integer(window_strides),
      dtype = "i64",
      shape = c()
    ),
    constant_attr(
      "base_dilations",
      as.integer(base_dilations),
      dtype = "i64",
      shape = c()
    ),
    constant_attr(
      "window_dilations",
      as.integer(window_dilations),
      dtype = "i64",
      shape = c()
    ),
    constant_attr(
      "padding",
      `storage.mode<-`(padding, "integer"),
      dtype = "i64",
      shape = dim(padding),
      simplify_dense = FALSE
    )
  )

  hlo_reduce_window_impl(
    values = c(inputs, init_values),
    funcs = list(body = body),
    attrs = attrs
  )
}
