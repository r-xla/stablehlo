#' @include types.R
#' @include repr.R
NULL

#' @title Constant
#' @description
#' This represents a constant value.
#' @param data (any)\cr
#'   The value of the constant.
#' @param type ([`TensorType`])\cr
#'   The type of the constant.
#' @return `Constant`
#' @export
Constant <- function(data, type) {
  checkmate::assert_class(type, "TensorType")

  structure(
    list(data = data, type = type),
    class = "Constant"
  )
}

#' @export
repr.Constant <- function(x, simplify_dense = TRUE, ...) {
  data <- x$data
  type <- x$type

  value_reprs <- if (test_class(data, "PJRTBuffer")) {
    pjrt::format_buffer(data)
  } else {
    if (test_class(type$dtype, "FloatType")) {
      format_double(
        data,
        precision = type$dtype$value
      )
    } else if (
      test_class(type$dtype, "IntegerType") ||
        test_class(type$dtype, "UnsignedType")
    ) {
      as.character(data)
    } else if (test_class(type$dtype, "BooleanType")) {
      tolower(as.logical(data))
    }
  }

  data_dims <- x$type$shape$dims

  if (simplify_dense) {
    if (length(data_dims) > 1) {
      cli_abort("Can only simplify dense mode for 1D arrays")
    }
    if (length(value_reprs) == 0) {
      return(paste0(
        "array<",
        repr(type$dtype),
        ">"
      ))
    } else {
      return(paste0(
        "array<",
        repr(type$dtype),
        ": ",
        paste(value_reprs, collapse = ", "),
        ">"
      ))
    }
  }

  # otherwise stableHLO parser error (use dense<> instead of e.g. dense<[[], []])
  if (length(value_reprs) == 0) {
    # empty array
    return(paste0(
      "dense<> : ",
      repr(type)
    ))
  }

  if (length(data_dims) == 0) {
    # scalar
    return(paste0(
      "dense<",
      paste(value_reprs, collapse = ", "),
      "> : ",
      repr(type)
    ))
  }

  if (length(value_reprs) == 1) {
    return(paste0(
      "dense<",
      value_reprs,
      "> : ",
      repr(type)
    ))
  }

  dim2 <- function(d) {
    if (is.array(d)) {
      dim(d)
    } else {
      length(d)
    }
  }

  if (!test_class(data, "PJRTBuffer")) {
    dim(value_reprs) <- dim2(data)
  }

  f <- function(x) {
    if (is.vector(x) || length(dim2(x)) == 1L) {
      paste0("[", paste(x, collapse = ", "), "]")
    } else {
      paste0("[", paste(apply(x, 1, f), collapse = ", "), "]")
    }
  }

  paste0("dense<", f(value_reprs), "> : ", repr(type))
}

#' @title Convert R value to Constant
#' @description
#' Convert R value to Constant.
#' @param value (any)\cr
#'   The value to convert.
#' @param dtype (`character(1)`)\cr
#'   The dtype of the constant.
#' @param shape (`integer()`)\cr
#'   The shape of the constant.
#' @param ... (any)
#'   Additional arguments.
#' @export
r_to_constant <- function(value, dtype = NULL, shape, ...) {
  UseMethod("r_to_constant")
}

#' @export
r_to_constant.default <- function(value, dtype = NULL, shape, ...) {
  cli_abort("Unsupported type for r_to_constant: {class(value)[1]}")
}

#' @export
r_to_constant.logical <- function(value, dtype = NULL, shape, ...) {
  if (!is.null(dtype) && !(dtype %in% c("i1", "pred"))) {
    cli_abort("Invalid dtype for logical")
  }
  shape <- Shape(shape)

  tensor_type <- TensorType(dtype = BooleanType(), shape = shape)

  return(Constant(data = value, type = tensor_type))
}

#' @export
r_to_constant.double <- function(value, dtype = NULL, shape, ...) {
  dtype <- dtype %??% "f32"
  if (dtype %in% c("i8", "i16", "i32", "i64", "ui8", "ui16", "ui32", "ui64")) {
    value <- as.integer(value)
  } else if (dtype %in% c("i1", "pred")) {
    cli_abort("Invalid dtype for double")
  }
  dtype <- as_dtype(dtype)
  shape <- Shape(shape)
  tensor_type <- TensorType(dtype, shape)

  return(Constant(data = value, type = tensor_type))
}

#' @export
r_to_constant.integer <- function(value, dtype = NULL, shape, ...) {
  dtype <- dtype %??% "i32"
  if (dtype %in% c("i1", "pred")) {
    cli_abort("Invalid dtype for integer")
  }
  if (dtype %in% c("f32", "f64")) {
    value <- as.double(value)
  }

  dtype <- as_dtype(dtype)

  shape <- Shape(shape)

  tensor_type <- TensorType(dtype, shape)

  return(Constant(data = value, type = tensor_type))
}

#' @export
r_to_constant.PJRTBuffer <- function(value, dtype = NULL, shape, ...) {
  dtype <- dtype %??% as.character(pjrt::elt_type(value))
  shape <- Shape(shape)

  tensor_type <- TensorType(dtype = as_dtype(dtype), shape = shape)

  return(Constant(data = value, type = tensor_type))
}

#' @export
#' @method shape Constant
shape.Constant <- function(x, ...) {
  x$type$shape$dims
}
