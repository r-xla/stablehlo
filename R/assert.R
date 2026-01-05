assert_valid_name <- function(name, arg = rlang::caller_arg(name)) {
  assert_string(
    name,
    pattern = "(^[a-zA-Z][a-zA-Z0-9_]*$)|(^[0-9]+$)",
    .var.name = arg
  )
}

assert_vt_equal <- function(
  x,
  y,
  ...,
  msg = NULL,
  is_tensor = TRUE,
  arg_x = rlang::caller_arg(x),
  arg_y = rlang::caller_arg(y)
) {
  rlang::check_dots_empty()

  if (is_tensor) {
    assert_vts_are_tensors(x, y)
  }

  if (x == y) {
    return()
  }

  cli_abort(c(
    x = msg %||% "Expected {.arg {arg_x}} and {.arg {arg_y}} to be equal.",
    i = "Got {.val {repr(x)}} and {.val {repr(y)}}."
  ))
}

assert_one_of <- function(x, ..., arg = rlang::caller_arg(x)) {
  # TODO: implement proper S3 type checking
  invisible(NULL)
}

assert_vt_is_tensor <- function(x, arg = rlang::caller_arg(x)) {
  force(arg)
  if (!inherits(x, "stablehlo_ValueType")) {
    cli_abort(c(
      "{.arg {arg}} must be a ValueType.",
      x = "Got {.class {class(x)[1]}}."
    ))
  }
  x <- x$type
  if (!inherits(x, "stablehlo_TensorType")) {
    cli_abort(c(
      "{.arg {arg}} must contain a TensorType.",
      x = "Got {.class {class(x)[1]}}."
    ))
  }
}

assert_vts_are_tensors <- function(...) {
  args <- list(...)
  arg_names <- names(args)
  if (is.null(arg_names) || all(arg_names == "")) {
    arg_names <- vapply(
      substitute(list(...))[-1],
      function(x) paste(deparse(x), collapse = " "),
      character(1)
    )
  }
  for (i in seq_along(args)) {
    assert_vt_is_tensor(args[[i]], arg = arg_names[i])
  }
}

assert_vt_has_ttype <- function(
  x,
  ...,
  shape = NULL,
  arg = rlang::caller_arg(x)
) {
  force(arg)
  if (!inherits(x, "stablehlo_ValueType")) {
    cli_abort(c(
      "{.arg {arg}} must be a ValueType.",
      x = "Got {.class {class(x)[1]}}."
    ))
  }
  tensor_type <- x$type
  if (!inherits(tensor_type, "stablehlo_TensorType")) {
    cli_abort(c(
      "{.arg {arg}} must be a TensorType.",
      x = "Got {.val {repr(tensor_type)}}."
    ))
  }

  dtypes <- list(...)

  if (length(dtypes) > 0) {
    dtype_matched <- FALSE
    type_names <- character(length(dtypes))

    for (i in seq_along(dtypes)) {
      dtype <- dtypes[[i]]
      # Get the class name - handle both instances and class name strings
      if (is.character(dtype)) {
        dtype_class <- dtype
        type_names[i] <- dtype
      } else if (is.function(dtype)) {
        # Skip function checks for now
        dtype_matched <- TRUE
        break
      } else {
        dtype_class <- class(dtype)[1]
        type_names[i] <- dtype_class
      }
      if (inherits(tensor_type$dtype, dtype_class)) {
        dtype_matched <- TRUE
        break
      }
    }

    if (!dtype_matched) {
      cli_abort(c(
        "{.arg {arg}} must be one of {.or {type_names}}.",
        x = "Got {.class {class(tensor_type$dtype)[1]}}."
      ))
    }
  }

  # fmt: skip
  repr_shape <- function(s) { # nolint
    paste0("(", s, collapse = ",", ")")
  }

  if (!is.null(shape) && !identical(stablehlo::shape(tensor_type), shape)) {
    cli_abort(c(
      "{.arg {arg}} must have shape {repr_shape(shape)}.",
      x = "Got {repr_shape(stablehlo::shape(tensor_type))}."
    ))
  }
}


assert_vts_have_same_dtype <- function(x, y, arg = rlang::caller_arg(x)) {
  # Use direct class comparison to avoid S3 dispatch issues with different types
  dtype_x <- x$type$dtype
  dtype_y <- y$type$dtype
  same <- class(dtype_x)[1] == class(dtype_y)[1]
  if (same && inherits(dtype_x, "stablehlo_IntegerType")) {
    same <- dtype_x$value == dtype_y$value
  } else if (same && inherits(dtype_x, "stablehlo_UnsignedType")) {
    same <- dtype_x$value == dtype_y$value
  } else if (same && inherits(dtype_x, "stablehlo_FloatType")) {
    same <- dtype_x$value == dtype_y$value
  }
  if (!same) {
    cli_abort(c(
      "{.arg {arg}} must have the same dtype.",
      x = "Got {.class {class(dtype_x)[1]}} and {.class {class(dtype_y)[1]}}."
    ))
  }
}
