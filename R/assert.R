assert_tensor_constant <- function(
  x,
  ndims = NULL,
  dtype = NULL,
  null_ok = FALSE,
  arg = rlang::caller_arg(x)
) {
  if (is.null(x) && null_ok) {
    return()
  }
  if (!inherits(x, Constant)) {
    cli_abort(c(
      "{.arg {arg}} must be an: {.cls anvil::Constant}.",
      x = "Got {.cls {class(x)[1]}}."
    ))
  }
  if (!inherits(x@value, TensorConstant)) {
    cli_abort(c(
      "{.arg {arg}} must wrap a {.cls anvil::TensorConstant}.",
      x = "Got {.cls {class(x@value)[1]}}."
    ))
  }
  if (!is.null(ndims) && length(shape(x@value)) != ndims) {
    cli_abort(c(
      "{.arg {arg}} must have {.val {ndims}} dimensions.",
      x = "Got {.val {length(shape(x@value))}} dimensions."
    ))
  }

  if (!is.null(dtype) && repr(x@value@type@dtype) != dtype) {
    cli_abort(c(
      "{.arg {arg}} must have element type {.val {dtype}}.",
      x = "Got {.val {repr(x@value@type@dtype)}}."
    ))
  }
}


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
  arg_x = rlang::caller_arg(x),
  arg_y = rlang::caller_arg(y)
) {
  rlang::check_dots_empty()

  if (x == y) {
    return()
  }

  cli_abort(c(
    x = msg %||% "Expected {.arg {arg_x}} and {.arg {arg_y}} to be equal.",
    i = "Got {.val {repr(x)}} and {.val {repr(y)}}."
  ))
}

assert_inherits_one_of <- function(x, ..., arg = rlang::caller_arg(x)) {
  types <- list(...)
  for (type in types) {
    if (inherits(x, type)) {
      return(invisible(NULL))
    }
  }

  type_names <- vapply(
    # nolint
    types,
    function(t) {
      if (inherits(t, "S7_class")) {
        return(t@name)
      }
      if (is.character(t)) {
        return(t)
      }
      "<unknown>"
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
  if (!inherits(x, ValueType)) {
    cli_abort(c(
      "{.arg {arg}} must be a ValueType.",
      x = "Got {.class {class(x)[1]}}."
    ))
  }
  x <- x@type
  if (!inherits(x, TensorType)) {
    cli_abort(c(
      "{.arg {arg}} must contain a TensorType.",
      x = "Got {.class {S7::S7_class(x)@name}}."
    ))
  }
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

assert_vt_has_dtype <- function(x, ..., arg = rlang::caller_arg(x)) {
  if (!inherits(x@type, TensorType)) {
    cli_abort(c(
      "{.arg {arg}} must be a tensor to have a dtype.",
      x = "Got {.val {repr(x@type)}}."
    ))
  }

  assert_inherits_one_of(x@type@dtype, ..., arg = paste0("dtype(", arg, ")"))
}

assert_vt_has_ttype <- function(
  x,
  ...,
  shape = NULL,
  arg = rlang::caller_arg(x)
) {
  if (!inherits(x, ValueType)) {
    cli_abort(c(
      "{.arg {arg}} must be a ValueType.",
      x = "Got {.class {class(x)[1]}}."
    ))
  }
  force(arg)
  x <- x@type
  if (!inherits(x, TensorType)) {
    cli_abort(c(
      "{.arg {arg}} must be a tensor to have a type.",
      x = "Got {.val {repr(x)}}."
    ))
  }

  dtypes <- list(...)

  # Filter out TensorType (it's already checked above)
  dtypes <- Filter(function(dt) !identical(dt, TensorType), dtypes)

  # If there are actual dtypes to check, verify the dtype matches at least one
  if (length(dtypes) > 0) {
    type_names <- vapply(dtypes, \(dt) dt@name, character(1)) # nolint

    dtype_matched <- FALSE
    for (dtype in dtypes) {
      if (inherits(x@dtype, dtype)) {
        dtype_matched <- TRUE
        break
      }
    }

    if (!dtype_matched) {
      cli_abort(c(
        "{.arg {arg}} must be one of {.or {type_names}}.",
        x = "Got {.class {S7::S7_class(x@dtype)@name}}."
      ))
    }
  }

  repr_shape <- function(s) {
    # nolint
    paste0("(", s, collapse = ",", ")")
  }

  if (!is.null(shape) && !identical(stablehlo::shape(x), shape)) {
    cli_abort(c(
      "{.arg {arg}} must have shape {repr_shape(shape)}.",
      x = "Got {repr_shape(stablehlo::shape(x))}."
    ))
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
