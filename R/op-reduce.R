#' @include op.R hlo.R type_inference.R
NULL

OpReduce <- new_Op("OpReduce", "reduce")

infer_types_reduce <- function(..., body, dimensions, num_inputs) {
  value_types <- list(...)

  if (length(value_types) != 2L * num_inputs) {
    stop(
      "Number of values must be 2 * num_inputs (inputs + init_values)"
    )
  }

  # Split inputs and init_values
  input_value_types <- value_types[seq_len(num_inputs)]
  init_value_types <- value_types[seq_len(num_inputs) + num_inputs]

  # Validate tensor types and shapes
  lapply(input_value_types, function(vt) {
    stopifnot(inherits(vt@type, TensorType))
  })
  lapply(init_value_types, function(vt) {
    stopifnot(inherits(vt@type, TensorType))
    stopifnot(length(vt@type@shape@dims) == 0L)
  })

  # All inputs must have the same shape
  input_shapes <- lapply(input_value_types, function(vt) shape(vt))
  ref_shape <- input_shapes[[1L]]
  if (
    !all(vapply(input_shapes, function(s) identical(s, ref_shape), logical(1L)))
  ) {
    stop("All inputs to reduce must have the same shape")
  }

  # (C2) element types must match between inputs and init_values (per position)
  for (i in seq_len(num_inputs)) {
    if (input_value_types[[i]]@type@dtype != init_value_types[[i]]@type@dtype) {
      stop("Element types of inputs and init_values must match")
    }
  }

  # Dimensions attribute: 0-based indices
  dims0 <- as.integer(dimensions@value@data)
  if (length(dims0) > 0L) {
    rank <- length(ref_shape)
    if (any(dims0 < 0L | dims0 >= rank)) {
      stop("dimensions must be within [0, rank(operand))")
    }
    if (any(duplicated(dims0))) {
      stop("dimensions must be unique")
    }
  }

  # Compute result shape: remove reduced axes
  result_dims <- if (length(dims0) == 0L) {
    ref_shape
  } else {
    ref_shape[-(dims0 + 1L)]
  }

  # Determine output element types from body outputs (C6, C8)
  body_out_types <- ValueTypes(
    lapply(body@outputs@items, function(x) x@type)
  )
  if (length(body_out_types@items) != num_inputs) {
    stop("Body must return one tensor per input")
  }

  # Build output ValueTypes with shapes after reduction
  out_vts <- lapply(seq_len(num_inputs), function(i) {
    out_elem_vt <- body_out_types@items[[i]]
    # Expect 0-D tensor element type; take dtype from it
    stopifnot(inherits(out_elem_vt@type, TensorType))
    ValueType(
      TensorType(
        dtype = out_elem_vt@type@dtype,
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
  # Normalize to lists
  inputs <- if (is.list(inputs)) inputs else list(inputs)
  init_values <- if (is.list(init_values)) init_values else list(init_values)

  if (length(inputs) != length(init_values)) {
    stop("inputs and init_values must have the same length")
  }

  # dimensions is an i64 1-D tensor attribute
  dim_attr <- hlo_tensor(
    as.integer(dimensions),
    dtype = "i64",
    func = Func()
  )

  hlo_reduce_impl(
    values = c(inputs, init_values),
    funcs = list(body = body),
    attrs = list(dimensions = dim_attr),
    custom_attrs = list(num_inputs = length(inputs))
  )
}

method(repr, OpReduce) <- function(x) {
  paste0(
    repr(x@outputs),
    " = ",
    repr(x@name),
    " ",
    repr(x@inputs, simplify_dense = TRUE),
    ": ",
    repr(x@signature)
  )
}

#
#
#
