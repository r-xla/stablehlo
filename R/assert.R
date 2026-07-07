#' @importFrom stats setNames
NULL

is_valid_id <- function(name) {
  test_string(name, pattern = "(^[a-zA-Z][a-zA-Z0-9_]*$)|(^[0-9]+$)")
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
      x = "Got {.val {x$type}} and {.val {y$type}}."
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
    if (inherits(x, type)) {
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

assert_vts_are_tensors <- function(..., call = rlang::caller_env()) {
  args <- list(...)
  all_ok <- TRUE
  for (x in args) {
    if (!inherits(x, "ValueType") || !inherits(x$type, "TensorType")) {
      all_ok <- FALSE
      break
    }
  }
  if (all_ok) {
    return(invisible(NULL))
  }
  arg_names <- names(args)
  if (is.null(arg_names)) {
    for (i in seq_along(args)) {
      assert_vt_is_tensor(args[[i]], call = call)
    }
  } else {
    for (i in seq_along(args)) {
      assert_vt_is_tensor(args[[i]], arg = arg_names[i], call = call)
    }
  }
}

assert_vt_is_tensor <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (!inherits(x, "ValueType")) {
    cli_abort(
      c(
        "{.arg {arg}} must be a ValueType.",
        x = "Got {.cls {class(x)[1]}}."
      ),
      call = call
    )
  }
  tensor_type <- x$type
  if (!inherits(tensor_type, "TensorType")) {
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

  # Fast path for the common success case: class-name dtypes, no
  # shape/ndims constraint.
  if (
    is.null(shape) &&
      is.null(ndims) &&
      inherits(x, "ValueType") &&
      inherits(x$type, "TensorType")
  ) {
    if (length(dtypes) == 0L) {
      return(invisible(NULL))
    }
    dt <- x$type$dtype
    for (dtype in dtypes) {
      if (is.character(dtype) && inherits(dt, dtype)) {
        return(invisible(NULL))
      }
    }
  }

  if (!inherits(x, "ValueType")) {
    cli_abort(
      c(
        "{.arg {arg}} must be a ValueType.",
        x = "Got {.cls {class(x)[1]}}."
      ),
      call = call
    )
  }

  tensor_type <- x$type
  if (!inherits(tensor_type, "TensorType")) {
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
        # dtype is a class name string - use inherits
        type_names[i] <- dtype
        if (inherits(tensor_type$dtype, dtype)) {
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
          x = "Got {.val {tensor_type$dtype}}."
        ),
        call = call
      )
    }
  }

  if (!is.null(shape) && !identical(shape(tensor_type), shape)) {
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

  # dtype objects are canonically constructed lists, so identical() is an
  # exact and dispatch-free equality
  if (!identical(dtype_x, dtype_y)) {
    cli_abort(
      c(
        "{.arg {arg_x}} and {.arg {arg_y}} must have the same dtype.",
        x = "Got {.val {dtype_x}} and {.val {dtype_y}}."
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
  if (!inherits(x, "Func")) {
    cli_abort(
      c(
        "{.arg {arg}} must be a Func.",
        x = "Got {.cls {class(x)[1]}}."
      ),
      call = call
    )
  }
}
