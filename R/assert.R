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
  types <- list(...)
  for (type in types) {
    if (inherits(x, type)) {
      return(invisible(NULL))
    }
  }

  # fmt: skip
  type_names <- vapply( # nolint
    types,
    function(t) {
      return(t)
    },
    character(1)
  )

  cli_abort(c(
    "{.arg {arg}} must be a {.or {.cls {type_names}}}.",
    x = "Got {.cls {class(x)[1]}}."
  ))
}

assert_vt_is_tensor <- function(x, arg = rlang::caller_arg(x)) {
  force(arg)
  if (!test_class(x, "ValueType")) {
    cli_abort(c(
      "{.arg {arg}} must be a ValueType.",
      x = "Got {.class {class(x)[1]}}."
    ))
  }
  x <- x$type
  if (!test_class(x, "TensorType")) {
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
  if (!test_class(x, "ValueType")) {
    cli_abort(c(
      "{.arg {arg}} must be a ValueType.",
      x = "Got {.class {class(x)[1]}}."
    ))
  }
  tensor_type <- x$type
  if (!test_class(tensor_type, "TensorType")) {
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

      # dtype should be either a class name (string) or an initialized instance
      if (is.character(dtype)) {
        # dtype is a class name string - use test_class
        type_names[i] <- dtype
        if (test_class(tensor_type$dtype, dtype)) {
          dtype_matched <- TRUE
          break
        }
      } else {
        # dtype is an initialized instance - compare with identical
        type_names[i] <- repr(dtype)
        if (identical(tensor_type$dtype, dtype)) {
          dtype_matched <- TRUE
          break
        }
      }
    }

    if (!dtype_matched) {
      cli_abort(c(
        "{.arg {arg}} must be one of {.or {type_names}}.",
        x = "Got {repr(tensor_type$dtype)}."
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
  dtype_x <- x$type$dtype
  dtype_y <- y$type$dtype

  if (dtype_x != dtype_y) {
    cli_abort(c(
      "{.arg {arg}} must have the same dtype.",
      x = "Got {repr(dtype_x)} and {repr(dtype_y)}."
    ))
  }
}
