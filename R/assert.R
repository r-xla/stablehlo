#' @importFrom stats setNames
NULL

is_valid_id <- function(name) {
  test_string(name, pattern = "(^[a-zA-Z][a-zA-Z0-9_]*$)|(^[0-9]+$)")
}

dtype_repr <- function(dtype) {
  if (inherits(dtype, "S7_class")) {
    return(dtype@name)
  }
  if (inherits(dtype, BooleanType)) {
    return("BooleanType")
  }
  sprintf("%s(%d)", S7_class(dtype)@name, dtype@value)
}

assert_valid_id <- function(
  name,
  arg = rlang::caller_arg(name),
  call = sys.call(-1)
) {
  if (!is_valid_id(name)) {
    throw_error(InvalidIdentifierError(arg = arg), call = call)
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
  call = sys.call(-1)
) {
  rlang::check_dots_empty()

  if (is_tensor) {
    assert_vts_are_tensors(x, y)
  }

  if (x == y) {
    return()
  }

  unequal_tensor_types_error(
    args = setNames(list(x@type, y@type), c(arg_x, arg_y)),
    call = call
  )
}

assert_one_of <- function(
  x,
  ...,
  arg = rlang::caller_arg(x),
  call = sys.call(-1)
) {
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
      return(t@name)
    },
    character(1)
  )

  class_error(
    arg = arg,
    observed = class(x)[1],
    expected = type_names,
    call = call
  )
}

assert_vts_are_tensors <- function(...) {
  args <- list(...)
  arg_names <- names(args)
  if (is.null(arg_names)) {
    arg_names <- vapply(
      substitute(list(...))[-1],
      deparse,
      character(1)
    )
  }
  for (i in seq_along(args)) {
    assert_vt_is_tensor(args[[i]], arg = arg_names[i])
  }
}

assert_vt_is_tensor <- function(
  x,
  expected_dtypes = NULL,
  expected_shape = NULL,
  arg = rlang::caller_arg(x),
  call = sys.call(-1)
) {
  if (!is.null(expected_dtypes) && !is.list(expected_dtypes)) {
    expected_dtypes <- list(expected_dtypes)
  }
  force(arg)
  if (!inherits(x, ValueType)) {
    throw_error(
      ClassError(
        arg = arg,
        observed = class(x)[1],
        expected = "stablehlo::ValueType"
      ),
      call = call
    )
  }
  x <- x@type
  if (!inherits(x, TensorType)) {
    throw_error(
      ClassError(
        arg = sprintf("%s@value", arg),
        observed = S7::S7_class(x)@name,
        expected = "stablehlo::TensorType"
      ),
      call = call
    )
  }

  dtypes <- expected_dtypes

  if (length(dtypes) > 0) {
    # Extract type names, handling both classes and instances
    type_names <- vapply(dtypes, dtype_repr, character(1))

    dtype_matched <- FALSE
    for (dtype in dtypes) {
      # Check if dtype is a class or an instance
      if (inherits(dtype, "S7_class")) {
        # dtype is a class, use inherits
        if (inherits(x@dtype, dtype)) {
          dtype_matched <- TRUE
          break
        }
      } else {
        # dtype is an instance, check exact equality (not just class match)
        if (identical(x@dtype, dtype)) {
          dtype_matched <- TRUE
          break
        }
      }
    }

    if (!dtype_matched) {
      throw_error(
        TensorDTypeError(
          arg = arg,
          expected = type_names,
          observed = dtype_repr(x@dtype)
        )
      )
    }
  }

  if (
    !is.null(expected_shape) && !identical(stablehlo::shape(x), expected_shape)
  ) {
    tensor_shape_error(
      arg = arg,
      expected = expected_shape,
      observed = stablehlo::shape(x)
    )
  }
}


assert_vts_have_same_dtype <- function(x, y, arg = rlang::caller_arg(x)) {
  if (x@type@dtype != y@type@dtype) {
    cli_abort(c(
      "{.arg {arg}} must have the same dtype.",
      x = "Got {.class {S7::S7_class(x@type@dtype)@name}} and {.class {S7::S7_class(y@type@dtype)@name}}."
    ))
  }
}
