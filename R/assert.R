#' @importFrom stats setNames
NULL

# Category token -> predicate match for a DataType.
dtype_in_category <- function(dt, category) {
  switch(
    category,
    float = is_dtype_float(dt),
    int = is_dtype_int(dt),
    uint = is_dtype_uint(dt),
    bool = is_dtype_bool(dt),
    cli_abort("Unknown dtype category: {.val {category}}")
  )
}

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
  naxes = NULL,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  dtypes <- list(...)

  # Fast path for the common success case: class-name dtypes, no
  # shape/naxes constraint.
  if (
    is.null(shape) &&
      is.null(naxes) &&
      inherits(x, "ValueType") &&
      inherits(x$type, "TensorType")
  ) {
    if (length(dtypes) == 0L) {
      return(invisible(NULL))
    }
    dt <- x$type$dtype
    for (dtype in dtypes) {
      if (
        is.character(dtype) &&
          !inherits(dtype, "DataType") &&
          dtype_in_category(dt, dtype)
      ) {
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

      # dtype should be either a category token (plain string) or an
      # initialized DataType instance. DataType is itself a classed
      # character vector, so the instance check must come first.
      if (inherits(dtype, "DataType")) {
        # dtype is an initialized instance - compare with identical
        type_names[i] <- repr(dtype)
        if (identical(tensor_type$dtype, dtype)) {
          dtype_matched <- TRUE
          break
        }
      } else if (is.character(dtype)) {
        # dtype is a category token - use predicate match
        type_names[i] <- dtype
        if (dtype_in_category(tensor_type$dtype, dtype)) {
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
  if (!is.null(naxes) && naxes(tensor_type) != naxes) {
    cli_abort(
      c(
        "{.arg {arg}} must have {naxes} axes.",
        x = "Got {length(shape(tensor_type))} axes."
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
  naxes = NULL,
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
  if (!is.null(naxes) && naxes(x$type) != naxes) {
    cli_abort(
      c(
        "{.arg {arg}} must have {naxes} {cli::qty(naxes)}ax{?is/es}.",
        x = "Got {cli::qty(length(shape(x$type)))}{length(shape(x$type))}."
      ),
      call = call
    )
  }
  # Every inference function reads `x$data` and compares it -- a missing value
  # reaches the first `if ()` and comes back out as R's own
  # "missing value where TRUE/FALSE needed", with no argument named.
  if (anyNA(x$data)) {
    cli_abort(
      c(
        "{.arg {arg}} must not contain missing values.",
        x = "Got {vec_repr(x$data)}."
      ),
      call = call
    )
  }
  invisible(NULL)
}

assert_shapevec <- function(x, arg = rlang::caller_arg(x)) {
  assert_integerish(x, lower = 0, any.missing = FALSE, .var.name = arg)
}

# A result shape an inference rule computed from the caller's attributes. The
# arithmetic that produces it runs in double, because attributes that each sit
# inside the integer range still overflow when they are summed -- and the `NA`
# an overflow produces reaches the next `if ()` as R's own "missing value where
# TRUE/FALSE needed", under the op's name and with nothing to act on. A
# dimension that no longer fits an R integer is refused here rather than at
# `Shape()`, which would report it as a missing value the caller never wrote.
assert_result_dims <- function(dims, what, call = rlang::caller_env()) {
  int_max <- .Machine$integer.max
  bad <- which(dims > int_max)
  if (length(bad)) {
    cli_abort(
      c(
        "{what} must have at most {.val {int_max}} elements in each dimension.",
        x = "{cli::qty(length(bad))}Dimension{?s} {vec_repr(bad - 1L)} would be {vec_repr(dims[bad])}." # nolint
      ),
      call = call
    )
  }
  as.integer(dims)
}

# A vector of dimension numbers the caller supplied directly (the members of a
# GatherDimensionNumbers / ScatterDimensionNumbers). `as.integer()` alone lets a
# missing value or a character through to the first `if ()` that reads it, where
# it becomes R's own "missing value where TRUE/FALSE needed".
#
# `checkmate::test_integerish()` is what decides "whole number": it covers the
# missing value, the fraction and the wrong type, and also the value that is
# whole but outside the integer range (`3e9`, `Inf`), which `as.integer()` would
# quietly turn into the very `NA` this guard exists to keep out.
assert_dimvec <- function(
  x,
  len = NULL,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (!checkmate::test_integerish(x, any.missing = FALSE)) {
    cli_abort(
      c(
        "{.arg {arg}} must contain whole numbers without missing values.",
        x = "Got {.cls {class(x)[1]}} {vec_repr(x)}."
      ),
      call = call
    )
  }
  if (!is.null(len) && length(x) != len) {
    cli_abort(
      c(
        "{.arg {arg}} must have {len} entr{cli::qty(len)}{?y/ies}.",
        x = "Got {length(x)}."
      ),
      call = call
    )
  }
  as.integer(x)
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

assert_dtype_one_of <- function(
  x,
  categories,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  for (category in categories) {
    if (dtype_in_category(x, category)) {
      return(invisible(NULL))
    }
  }
  cli_abort(
    c(
      "{.arg {arg}} must have a {.or {categories}} dtype.",
      x = "Got {.val {x}}."
    ),
    call = call
  )
}

is_promotable_dtype <- function(x, y) {
  same_class <- (is_dtype_bool(x) && is_dtype_bool(y)) ||
    ((is_dtype_int(x) || is_dtype_uint(x)) &&
      (is_dtype_int(y) || is_dtype_uint(y))) ||
    (is_dtype_float(x) && is_dtype_float(y))
  if (!same_class) {
    return(FALSE)
  }
  if (is_dtype_bool(x)) {
    return(TRUE)
  }
  dtype_width(x) <= dtype_width(y)
}

assert_region_inputs <- function(
  region,
  dtypes,
  arg = "body",
  interleaved = FALSE,
  dtype_label = "the accumulator's element types",
  call = rlang::caller_env()
) {
  n <- length(dtypes)
  in_types <- lapply(region$inputs, function(x) x$type)
  if (length(in_types) != 2L * n) {
    cli_abort(
      c(
        "{.arg {arg}} must take two arguments per input.",
        x = "Expected {2L * n} argument{?s}, got {length(in_types)}."
      ),
      call = call
    )
  }
  expected <- if (interleaved) rep(dtypes, each = 2L) else rep(dtypes, 2L)
  for (i in seq_along(expected)) {
    vt <- in_types[[i]]
    if (!inherits(vt$type, "TensorType") || length(shape(vt)) != 0L) {
      cli_abort(
        c(
          "{.arg {arg}} arguments must be 0-dimensional tensors.",
          x = "Argument {i - 1L} has type {.val {vt$type}}."
        ),
        call = call
      )
    }
    if (vt$type$dtype != expected[[i]]) {
      cli_abort(
        c(
          "{.arg {arg}} arguments must have {dtype_label}.",
          x = "Argument {i - 1L} has type {.val {vt$type$dtype}}, expected
               {.val {expected[[i]]}}."
        ),
        call = call
      )
    }
  }
  invisible(NULL)
}

# reduce (C6) / reduce_window (C13) / scatter (C23): the accumulator element
# type `Ei` the body reduces into must be a widening of the input's, not
# necessarily equal to it.
assert_accumulator_dtypes <- function(
  input_dtypes,
  accumulator_dtypes,
  arg = "body",
  call = rlang::caller_env()
) {
  for (i in seq_along(input_dtypes)) {
    if (!is_promotable_dtype(input_dtypes[[i]], accumulator_dtypes[[i]])) {
      cli_abort(
        c(
          "{.arg {arg}} must reduce into a type its input promotes to.",
          x = "Input {i - 1L} has type {.val {input_dtypes[[i]]}}, which does
               not promote to {.val {accumulator_dtypes[[i]]}}."
        ),
        call = call
      )
    }
  }
  invisible(NULL)
}
