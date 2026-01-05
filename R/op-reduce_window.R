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
  assert_vts_are_tensors(...)
  value_types <- list(...)

  # (C1)
  if (length(value_types) %% 2L != 0L) {
    cli_abort(
      "Number of arguments must be divisible by 2 (pairs of inputs and init values)"
    )
  }
  if (length(value_types) == 0L) {
    cli_abort("No arguments provided")
  }

  num_inputs <- length(value_types) / 2L

  input_value_types <- value_types[seq_len(num_inputs)]
  init_value_types <- value_types[seq_len(num_inputs) + num_inputs]

  # Check init_values are 0-D tensors

  lapply(init_value_types, function(vt) {
    if (length(vt$type$shape$dims) != 0L) {
      cli_abort("init_values must be 0-D tensors")
    }
  })

  # (C2)
  input_shapes <- lapply(input_value_types, function(vt) shape(vt))
  ref_shape <- input_shapes[[1L]]
  if (
    !all(vapply(input_shapes, function(s) identical(s, ref_shape), logical(1L)))
  ) {
    cli_abort("All inputs to reduce_window must have the same shape")
  }

  # (C3) Each input must have the same dtype as its corresponding init_value
  for (i in seq_len(num_inputs)) {
    input_dtype <- input_value_types[[i]]$type$dtype
    init_dtype <- init_value_types[[i]]$type$dtype
    if (input_dtype != init_dtype) {
      cli_abort(
        "inputs[{i}] and init_values[{i}] must have the same dtype, got: {repr(input_dtype)} and {repr(init_dtype)}."
      )
    }
  }

  rank <- length(ref_shape)
  window_dims <- as.integer(window_dimensions$value$data)

  # (C4)
  if (length(window_dims) != rank) {
    cli_abort("window_dimensions must have length equal to input rank")
  }

  # (C5)
  if (any(window_dims < 1L)) {
    cli_abort("window_dimensions must be positive")
  }

  strides <- as.integer(window_strides$value$data)
  base_dil <- as.integer(base_dilations$value$data)
  window_dil <- as.integer(window_dilations$value$data)
  pad <- padding$value$data

  # (C12)
  if (!is.matrix(pad) || !is.integer(pad)) {
    cli_abort("padding must be a 2D integer matrix")
  }
  if (nrow(pad) != rank || ncol(pad) != 2L) {
    cli_abort("padding must have shape [rank, 2]")
  }

  # (C6)
  if (length(strides) != rank) {
    cli_abort("window_strides must have length equal to input rank")
  }

  # (C7)
  if (any(strides < 1L)) {
    cli_abort("window_strides must be positive")
  }

  # (C8)
  if (length(base_dil) != rank) {
    cli_abort("base_dilations must have length equal to input rank")
  }
  # (C10)
  if (length(window_dil) != rank) {
    cli_abort("window_dilations must have length equal to input rank")
  }

  # (C9)
  if (any(base_dil <= 0)) {
    cli_abort("base_dilations must be positive")
  }
  # (C11)
  if (any(window_dil < 0)) {
    cli_abort("window_dilations must be positive")
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
    cli_abort("Body must return one tensor per input")
  }

  out_vts <- lapply(seq_len(num_inputs), function(i) {
    out_elem_vt <- body_out_types[[i]]
    if (length(out_elem_vt$type$shape$dims) != 0L) {
      cli_abort("body outputs must be 0-D tensors")
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
