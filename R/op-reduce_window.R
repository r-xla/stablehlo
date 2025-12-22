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
  window_strides = NULL,
  base_dilations = NULL,
  window_dilations = NULL,
  padding = NULL
) {
  assert_vts_are_tensors(...)
  value_types <- list(...)

  if (length(value_types) %% 2L != 0L) {
    cli_abort("Number of arguments must be divisible by 2")
  }
  if (length(value_types) == 0L) {
    cli_abort("No arguments provided")
  }

  num_inputs <- length(value_types) / 2L

  input_value_types <- value_types[seq_len(num_inputs)]
  init_value_types <- value_types[seq_len(num_inputs) + num_inputs]

  # Check init_values are 0-D tensors

  lapply(init_value_types, function(vt) {
    if (length(vt@type@shape@dims) != 0L) {
      cli_abort("init_values must be 0-D tensors")
    }
  })

  # All inputs must have the same shape
  input_shapes <- lapply(input_value_types, function(vt) shape(vt))
  ref_shape <- input_shapes[[1L]]
  if (
    !all(vapply(input_shapes, function(s) identical(s, ref_shape), logical(1L)))
  ) {
    cli_abort("All inputs to reduce_window must have the same shape")
  }

  # element types must match between inputs and init_values (per position)
  for (i in seq_len(num_inputs)) {
    if (input_value_types[[i]]@type@dtype != init_value_types[[i]]@type@dtype) {
      cli_abort("Element types of inputs and init_values must match")
    }
  }

  rank <- length(ref_shape)
  window_dims <- as.integer(window_dimensions@value@data)

  if (length(window_dims) != rank) {
    cli_abort("window_dimensions must have length equal to input rank")
  }

  # Default values for optional parameters
  strides <- if (!is.null(window_strides)) {
    as.integer(window_strides@value@data)
  } else {
    rep(1L, rank)
  }

  base_dil <- if (!is.null(base_dilations)) {
    as.integer(base_dilations@value@data)
  } else {
    rep(1L, rank)
  }

  window_dil <- if (!is.null(window_dilations)) {
    as.integer(window_dilations@value@data)
  } else {
    rep(1L, rank)
  }

  # Padding: should be Rx2 matrix, defaults to all zeros
  pad <- if (!is.null(padding)) {
    matrix(as.integer(padding@value@data), nrow = rank, ncol = 2L, byrow = TRUE)
  } else {
    matrix(0L, nrow = rank, ncol = 2L)
  }

  # Validation
  if (length(strides) != rank) {
    cli_abort("window_strides must have length equal to input rank")
  }

  if (length(base_dil) != rank) {
    cli_abort("base_dilations must have length equal to input rank")
  }
  if (length(window_dil) != rank) {
    cli_abort("window_dilations must have length equal to input rank")
  }
  if (nrow(pad) != rank || ncol(pad) != 2L) {
    cli_abort("padding must have shape [rank, 2]")
  }

  if (any(window_dims < 1L)) {
    cli_abort("window_dimensions must be positive")
  }
  if (any(strides < 1L)) {
    cli_abort("window_strides must be positive")
  }
  if (any(base_dil < 1L)) {
    cli_abort("base_dilations must be positive")
  }
  if (any(window_dil < 1L)) {
    cli_abort("window_dilations must be positive")
  }

  # Compute output shape:
  # dilated_input_shape[d] = (input_shape[d] - 1) * base_dilations[d] + 1
  # padded_input_shape[d] = dilated_input_shape[d] + padding[d,1] + padding[d,2]
  # dilated_window_shape[d] = (window_dims[d] - 1) * window_dilations[d] + 1
  # output_shape[d] = floor((padded_input_shape[d] - dilated_window_shape[d]) / strides[d]) + 1

  dilated_input <- (ref_shape - 1L) * base_dil + 1L
  padded_input <- dilated_input + pad[, 1L] + pad[, 2L]
  dilated_window <- (window_dims - 1L) * window_dil + 1L
  result_shape <- floor((padded_input - dilated_window) / strides) + 1L
  result_shape <- as.integer(result_shape)

  # Determine output element types from body outputs
  body_out_types <- ValueTypes(func_output_types(body))
  if (length(body_out_types@items) != num_inputs) {
    cli_abort("Body must return one tensor per input")
  }

  # Build output ValueTypes with computed output shapes
  out_vts <- lapply(seq_len(num_inputs), function(i) {
    out_elem_vt <- body_out_types@items[[i]]
    # Expect 0-D tensor element type; take dtype from it
    if (length(out_elem_vt@type@shape@dims) != 0L) {
      cli_abort("body outputs must be 0-D tensors")
    }
    ValueType(
      TensorType(
        dtype = out_elem_vt@type@dtype,
        shape = Shape(result_shape)
      )
    )
  })

  ValueTypes(out_vts)
}

hlo_reduce_window_impl <- hlo_fn(OpReduceWindow, infer_types_reduce_window)

#' @templateVar mnemonic reduce_window
#' @template op
#' @param inputs (`FuncValue` or `list` of `FuncValue`)\cr
#'   The input tensor(s) to apply the reduction to.
#' @param init_values (`FuncValue` or `list` of `FuncValue`)\cr
#'   The initial value(s) for the reduction. Must be 0-D tensors.
#' @param window_dimensions (`integer()`)\cr
#'   The size of the window in each dimension.
#' @param window_strides (`integer()` or `NULL`)\cr
#'   The stride of the window in each dimension. Defaults to 1 for each dimension.
#' @param base_dilations (`integer()` or `NULL`)\cr
#'   The dilation factor for the input tensor. Defaults to 1 for each dimension.
#' @param window_dilations (`integer()` or `NULL`)\cr
#'   The dilation factor for the window. Defaults to 1 for each dimension.
#' @param padding (`matrix` or `NULL`)\cr
#'   A matrix with shape `[rank, 2]` specifying the padding before and after
#'   each dimension. Defaults to no padding.
#' @param body (`Func`)\cr
#'   The reduction function to apply to each window.
#' @export
hlo_reduce_window <- function(
  inputs,
  init_values,
  window_dimensions,
  body,
  window_strides = NULL,
  base_dilations = NULL,
  window_dilations = NULL,
  padding = NULL
) {
  # Normalize inputs and init_values to lists
  if (!is.list(inputs)) {
    inputs <- list(inputs)
  }
  if (!is.list(init_values)) {
    init_values <- list(init_values)
  }

  rank <- length(shape(inputs[[1L]]@value_type))
  window_dimensions <- as.integer(window_dimensions)

  attrs <- list(
    constant_attr(
      "window_dimensions",
      window_dimensions,
      dtype = "i64",
      shape = c()
    )
  )

  if (!is.null(window_strides)) {
    attrs <- c(attrs, list(
      constant_attr(
        "window_strides",
        as.integer(window_strides),
        dtype = "i64",
        shape = c()
      )
    ))
  }

  if (!is.null(base_dilations)) {
    attrs <- c(attrs, list(
      constant_attr(
        "base_dilations",
        as.integer(base_dilations),
        dtype = "i64",
        shape = c()
      )
    ))
  }

  if (!is.null(window_dilations)) {
    attrs <- c(attrs, list(
      constant_attr(
        "window_dilations",
        as.integer(window_dilations),
        dtype = "i64",
        shape = c()
      )
    ))
  }

  if (!is.null(padding)) {
    # Convert padding matrix to 2D constant
    # padding should be [rank x 2] matrix
    if (is.matrix(padding)) {
      pad_data <- as.integer(t(padding))
      pad_shape <- c(nrow(padding), 2L)
    } else {
      # Assume it's a flat vector in row-major order
      pad_data <- as.integer(padding)
      pad_shape <- c(rank, 2L)
    }
    attrs <- c(attrs, list(
      ConstantAttr(
        name = "padding",
        value = Constant(
          value = TensorConstant(
            data = pad_data,
            type = TensorType(
              dtype = IntegerType(64L),
              shape = Shape(pad_shape)
            )
          )
        ),
        simplify_dense = FALSE
      )
    ))
  }

  hlo_reduce_window_impl(
    values = c(inputs, init_values),
    funcs = list(body = body),
    attrs = attrs
  )
}

