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

  args <- list(x, y)
  names(args) <- c(arg_x, arg_y)
  error_unequal_tensor_types(args)
}

assert_one_of <- function(x, ..., arg = rlang::caller_arg(x)) {
  types <- list(...)
  for (type in types) {
    if (inherits(x, type)) {
      return(invisible(NULL))
    }
  }

  error_class(arg, unlist(types), class(x)[1])
}

assert_vt_is_tensor <- function(x, arg = rlang::caller_arg(x)) {
  force(arg)
  if (!test_class(x, "ValueType")) {
    error_class(arg, "ValueType", class(x)[1])
  }
  x <- x$type
  if (!test_class(x, "TensorType")) {
    error_class(paste0(arg, "$type"), "TensorType", class(x)[1])
  }
}

assert_vts_are_tensors <- function(...) {
  args <- list(...)
  arg_names <- names(args)
  if (is.null(arg_names)) {
    for (i in seq_along(args)) {
      assert_vt_is_tensor(args[[i]])
    }
  } else {
    for (i in seq_along(args)) {
      assert_vt_is_tensor(args[[i]], arg = arg_names[i])
    }
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
    error_class(arg, "ValueType", class(x)[1])
  }
  tensor_type <- x$type
  if (!test_class(tensor_type, "TensorType")) {
    error_class(paste0(arg, "$type"), "TensorType", class(tensor_type)[1])
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
      error_tensor_dtype(arg, type_names, repr(tensor_type$dtype))
    }
  }

  if (!is.null(shape) && !identical(stablehlo::shape(tensor_type), shape)) {
    error_tensor_shape(arg, shape, stablehlo::shape(tensor_type))
  }
}


assert_vts_have_same_dtype <- function(
  x,
  y,
  arg_x = rlang::caller_arg(x),
  arg_y = rlang::caller_arg(y)
) {
  dtype_x <- x$type$dtype
  dtype_y <- y$type$dtype

  if (dtype_x != dtype_y) {
    args <- list(dtype_x, dtype_y)
    names(args) <- c(arg_x, arg_y)
    error_unequal_tensor_types(args)
  }
}
