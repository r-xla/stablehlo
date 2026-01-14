#' @include op.R hlo.R type_inference.R
NULL

OpReduce <- new_Op("OpReduce", "reduce")

#' @param ... (Inputs, Init values)
#' @rdname hlo_reduce
#' @export
infer_types_reduce <- function(..., body, dimensions) {
  assert_vts_are_tensors(...)
  assert_const(dimensions, dtype = IntegerType(64L), ndims = 1L)
  assert_func(body)
  value_types <- list(...)

  # (C3)
  if (length(value_types) %% 2L != 0L) {
    cli_abort(c(
      "Number of arguments must be divisible by 2",
      i = "Got {length(value_types)}."
    ))
  }
  if (length(value_types) == 0L) {
    cli_abort("No arguments provided")
  }

  num_inputs <- length(value_types) / 2L

  input_value_types <- value_types[seq_len(num_inputs)]
  init_value_types <- value_types[seq_len(num_inputs) + num_inputs]

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
      error_dimension_out_of_range(
        arg = "dimensions",
        dimension = dims0,
        dim_range = c(0L, num_dims)
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

hlo_reduce_impl <- hlo_fn(OpReduce, infer_types_reduce)

#' @templateVar mnemonic reduce
#' @template op
#' @export
hlo_reduce <- function(inputs, init_values, dimensions, body) {
  hlo_reduce_impl(
    values = c(inputs, init_values),
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
