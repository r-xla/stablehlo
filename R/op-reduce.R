#' @include op.R hlo.R type_inference.R
NULL

OpReduce <- new_Op("OpReduce", "reduce")

#' @rdname hlo_reduce
#' @export
infer_types_reduce <- function(inputs, init_values, body, dimensions) {
  inputs <- as.list(inputs)
  init_values <- as.list(init_values)

  lapply(inputs, assert_vt_is_tensor)
  lapply(init_values, assert_vt_is_tensor)
  assert_const(dimensions, dtype = IntegerType(64L), ndims = 1L)
  assert_func(body)

  # (C3)
  if (length(inputs) == 0L) {
    cli_abort("No inputs provided")
  }
  if (length(inputs) != length(init_values)) {
    cli_abort(c(
      "Number of inputs must equal number of init_values",
      x = "Got {length(inputs)} inputs and {length(init_values)} init_values."
    ))
  }

  num_inputs <- length(inputs)

  input_value_types <- inputs
  init_value_types <- init_values

  lapply(init_value_types, function(vt) {
    if (length(vt$type$shape$dims) != 0L) {
      cli_abort("init_values must be 0-D tensors")
    }
  })

  # (C1)
  input_shapes <- lapply(input_value_types, function(vt) shape(vt))
  ref_shape <- input_shapes[[1L]]
  if (
    !all(vapply(input_shapes, function(s) identical(s, ref_shape), logical(1L)))
  ) {
    cli_abort("All inputs to reduce must have the same shape")
  }

  # (C2)
  for (i in seq_len(num_inputs)) {
    if (input_value_types[[i]]$type$dtype != init_value_types[[i]]$type$dtype) {
      cli_abort("Element types of inputs and init_values must match")
    }
  }

  dims0 <- as.integer(dimensions$data)
  if (length(dims0) > 0L) {
    num_dims <- length(ref_shape)
    # (C4)
    if (any(dims0 < 0L | dims0 >= num_dims)) {
      error_index_out_of_bounds(
        arg = "dimensions",
        index = dims0,
        lower = 0L,
        upper = num_dims
      )
    }
    # (C5)
    if (anyDuplicated(dims0)) {
      error_dimension_uniqueness(
        arg = "dimensions",
        dimensions = dims0
      )
    }
  }

  # Determine output element types from body outputs (C6, C8)
  body_out_types <- ValueTypes(func_output_types(body))
  if (length(body_out_types) != num_inputs) {
    cli_abort(c(
      "Body must return one tensor per input",
      i = "Body returns {length(body_out_types)} tensors, but {num_inputs} are required."
    ))
  }
  for (i in seq_len(num_inputs)) {
    if (body_out_types[[i]]$type$dtype != input_value_types[[i]]$type$dtype) {
      cli_abort(c(
        "Body must return tensors with the same element type as the inputs"
      ))
    }
  }

  # (C7)
  result_dims <- if (length(dims0) == 0L) {
    ref_shape
  } else {
    ref_shape[-(dims0 + 1L)]
  }

  # (C8)
  out_vts <- lapply(seq_len(num_inputs), function(i) {
    out_elem_vt <- body_out_types[[i]]
    # Expect 0-D tensor element type; take dtype from it
    if (length(out_elem_vt$type$shape$dims) != 0L) {
      cli_abort("body outputs must be 0-D tensors")
    }
    ValueType(
      TensorType(
        dtype = out_elem_vt$type$dtype,
        shape = Shape(result_dims)
      )
    )
  })

  ValueTypes(out_vts)
}

hlo_reduce_impl <- hlo_fn(
  OpReduce,
  infer_types_reduce,
  value_list_names = c("inputs", "init_values")
)

#' @templateVar mnemonic reduce
#' @template op
#' @export
hlo_reduce <- function(inputs, init_values, dimensions, body) {
  if (inherits(inputs, "FuncValue")) {
    inputs <- list(inputs)
  } else if (!is.list(inputs)) {
    inputs <- list(inputs)
  }
  if (inherits(init_values, "FuncValue")) {
    init_values <- list(init_values)
  } else if (!is.list(init_values)) {
    init_values <- list(init_values)
  }
  hlo_reduce_impl(
    values = list(
      inputs = inputs,
      init_values = init_values
    ),
    funcs = list(body = body),
    attrs = list(
      constant_attr(
        "dimensions",
        as.integer(dimensions),
        shape = length(dimensions),
        dtype = "i64"
      )
    )
  )
}
