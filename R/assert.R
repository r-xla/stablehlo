#' @importFrom stats setNames
NULL

is_valid_id <- function(name) {
  test_string(name, pattern = "(^[a-zA-Z][a-zA-Z0-9_]*$)|(^[0-9]+$)")
}

dtype_repr <- function(dtype) {
  if (is.character(dtype)) {
    return(dtype)
  }
  # For initialized instances
  repr(dtype)
}

assert_valid_id <- function(
  name,
  arg = rlang::caller_arg(name),
  call = rlang::caller_env()
) {
  if (!is_valid_id(name)) {
    cli_abort(
      c(
        "Identifiers can only contain {{letters, digits, _}}; They must start with a letter or be all digits.",
        x = "{.arg {arg}} is {.val {name}}."
      ),
      call = call
    )
  }
}

assert_vt_equal <- function(
  x,
  y,
  ...,
  msg = NULL,
  is_tensor = TRUE,
  arg_x = rlang::caller_arg(x),
  arg_y = rlang::caller_arg(y),
  call = rlang::caller_env()
) {
  rlang::check_dots_empty()

  if (is_tensor) {
    assert_vts_are_tensors(x, y)
  }

  if (x == y) {
    return()
  }

  cli_abort(
    c(
      "{.arg {arg_x}} and {.arg {arg_y}} must have the same tensor type.",
      x = "Got {repr(x$type)} and {repr(y$type)}."
    ),
    call = call
  )
}

assert_one_of <- function(
  x,
  types,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  for (type in types) {
    if (test_class(x, type)) {
      return(invisible(NULL))
    }
  }

  cli_abort(
    c(
      "{.arg {arg}} must be a {.or {.cls {types}}}.",
      x = "Got {.cls {class(x)[1]}}."
    ),
    call = call
  )
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

assert_vt_is_tensor <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  force(arg)
  if (!test_class(x, "ValueType")) {
    cli_abort(
      c(
        "{.arg {arg}} must be a ValueType.",
        x = "Got {.cls {class(x)[1]}}."
      ),
      call = call
    )
  }
  tensor_type <- x$type
  if (!test_class(tensor_type, "TensorType")) {
    cli_abort(
      c(
        "{.arg {arg}} must contain a TensorType.",
        x = "Got {.cls {class(tensor_type)[1]}}."
      ),
      call = call
    )
  }
}

assert_vt_has_ttype <- function(
  x,
  ...,
  shape = NULL,
  ndims = NULL,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  dtypes <- list(...)
  force(arg)

  if (!test_class(x, "ValueType")) {
    cli_abort(
      c(
        "{.arg {arg}} must be a ValueType.",
        x = "Got {.cls {class(x)[1]}}."
      ),
      call = call
    )
  }

  tensor_type <- x$type
  if (!test_class(tensor_type, "TensorType")) {
    cli_abort(
      c(
        "{.arg {arg}} must contain a TensorType.",
        x = "Got {.cls {class(tensor_type)[1]}}."
      ),
      call = call
    )
  }

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
      cli_abort(
        c(
          "{.arg {arg}} must have dtype {.or {type_names}}.",
          x = "Got {.cls {repr(tensor_type$dtype)}}."
        ),
        call = call
      )
    }
  }

  if (!is.null(shape) && !identical(shape(tensor_type), shape)) {
    # fmt: skip
    shapevec_repr <- function(s) { # nolint
      sprintf("(%s)", paste0(s, collapse = ","))
    }
    cli_abort(
      c(
        "{.arg {arg}} must have shape {shapevec_repr(shape)}.",
        x = "Got {shapevec_repr(shape(tensor_type))}."
      ),
      call = call
    )
  }
  if (!is.null(ndims) && ndims(tensor_type) != ndims) {
    cli_abort(
      c(
        "{.arg {arg}} must have {ndims} dimensions.",
        x = "Got {length(shape(tensor_type))} dimensions."
      ),
      call = call
    )
  }
  invisible(NULL)
}

assert_vts_have_same_dtype <- function(
  x,
  y,
  arg_x = rlang::caller_arg(x),
  arg_y = rlang::caller_arg(y),
  call = rlang::caller_env()
) {
  dtype_x <- x$type$dtype
  dtype_y <- y$type$dtype

  if (dtype_x != dtype_y) {
    cli_abort(
      c(
        "{.arg {arg_x}} and {.arg {arg_y}} must have the same dtype.",
        x = "Got {.cls {repr(dtype_x)}} and {.cls {repr(dtype_y)}}."
      ),
      call = call
    )
  }
}

assert_const <- function(
  x,
  dtype = NULL,
  shape = NULL,
  ndims = NULL,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (!inherits(x, "Constant")) {
    cli_abort(
      c(
        "{.arg {arg}} must be a ConstantAttr.",
        x = "Got {.cls {class(x)[1]}}."
      ),
      call = call
    )
  }
  if (!is.null(dtype)) {
    dtype <- as_dtype(dtype)
    if (!identical(x$type$dtype, dtype)) {
      cli_abort(
        c(
          "{.arg {arg}} must have dtype {.cls {dtype}}.",
          x = "Got {.cls {x$type$dtype}}."
        ),
        call = call
      )
    }
  }
  if (!is.null(shape) && !identical(shape(x$type), shape)) {
    cli_abort(
      c(
        "{.arg {arg}} must have shape {shapevec_repr(shape)}.",
        x = "Got {shapevec_repr(shape(x$type))}."
      ),
      call = call
    )
  }
  if (!is.null(ndims) && ndims(x$type) != ndims) {
    cli_abort(
      c(
        "{.arg {arg}} must have {ndims} dimensions.",
        x = "Got {length(shape(x$type))} dimensions."
      ),
      call = call
    )
  }
  invisible(NULL)
}

assert_shapevec <- function(x) {
  assert_integerish(x, lower = 0, any.missing = FALSE)
}

assert_func <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (!test_class(x, "Func")) {
    cli_abort(
      c(
        "{.arg {arg}} must be a Func.",
        x = "Got {.cls {class(x)[1]}}."
      ),
      call = call
    )
  }
}
