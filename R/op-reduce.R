#' @include op.R hlo.R type_inference.R
NULL

OpReduce <- new_Op("OpReduce", "reduce")

#' @param ... (Inputs, Init values)
#' @rdname hlo_reduce
#' @export
infer_types_reduce <- function(..., body, dimensions) {
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
    cli_abort("All inputs to reduce must have the same shape")
  }

  # (C2) element types must match between inputs and init_values (per position)
  for (i in seq_len(num_inputs)) {
    if (input_value_types[[i]]@type@dtype != init_value_types[[i]]@type@dtype) {
      cli_abort("Element types of inputs and init_values must match")
    }
  }

  dims0 <- as.integer(dimensions@value@data)
  if (length(dims0) > 0L) {
    rank <- length(ref_shape)
    if (any(dims0 < 0L | dims0 >= rank)) {
      cli_abort("dimensions must be within [0, rank(operand))")
    }
    if (any(duplicated(dims0))) {
      cli_abort("dimensions must be unique")
    }
  }

  # Compute result shape: remove reduced axes
  result_dims <- if (length(dims0) == 0L) {
    ref_shape
  } else {
    ref_shape[-(dims0 + 1L)]
  }

  # Determine output element types from body outputs (C6, C8)
  body_out_types <- ValueTypes(func_output_types(body))
  if (length(body_out_types@items) != num_inputs) {
    cli_abort("Body must return one tensor per input")
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
  dim_attr <- hlo_tensor(
    as.integer(dimensions),
    dtype = "i64",
    func = Func()
  )
  hlo_reduce_impl(
    values = c(inputs, init_values),
    funcs = list(body = body),
    attrs = list(dimensions = dim_attr)
  )
}

method(repr, OpReduce) <- function(x, ...) {
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
