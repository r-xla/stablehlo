#' @include shape.R
NULL

#' @export
tengen::BooleanType

#' @export
tengen::IntegerType

#' @export
tengen::UIntegerType

#' @export
tengen::FloatType

#' @export
tengen::is_dtype

#' @export
tengen::as_dtype

#' @export
repr.BooleanType <- function(x, ...) {
  "i1"
}

#' @export
repr.IntegerType <- function(x, ...) {
  as.character(x)
}

#' @export
repr.UIntegerType <- function(x, ...) {
  as.character(x)
}

#' @export
repr.FloatType <- function(x, ...) {
  as.character(x)
}

# Re-export assert_dtype from tengen
assert_dtype <- tengen::assert_dtype

#' @title TensorType
#' @description
#' Represents a tensor type with a specific data type and shape.
#' @param dtype ([`DataType`])
#' @param shape ([`Shape`])
#' @return `TensorType`
#' @export
TensorType <- function(dtype, shape) {
  assert_dtype(dtype)
  checkmate::assert_class(shape, "Shape")

  structure(
    list(dtype = dtype, shape = shape),
    class = "TensorType"
  )
}

#' @export
`==.TensorType` <- function(e1, e2) {
  test_class(e2, "TensorType") &&
    e1$dtype == e2$dtype &&
    e1$shape == e2$shape
}

#' @export
`!=.TensorType` <- function(e1, e2) {
  !(e1 == e2) # nolint
}

#' @export
repr.TensorType <- function(x, ...) {
  shape_repr <- repr(x$shape)
  paste0("tensor<", shape_repr, if (nzchar(shape_repr)) "x", repr(x$dtype), ">")
}

#' @export
print.TensorType <- function(x, ...) {
  cat(repr(x), "\n")
  invisible(x)
}

#' @exportS3Method cli::cli_format
cli_format.TensorType <- function(x, style = NULL, ...) {
  repr(x)
}

#' @export
#' @method shape TensorType
shape.TensorType <- function(x, ...) {
  x$shape$dims
}

#' @export
#' @method dtype TensorType
dtype.TensorType <- function(x, ...) {
  x$dtype
}

# TokenType - internal, not exported
TokenType <- function() {
  structure(list(), class = "TokenType")
}

#' @export
repr.TokenType <- function(x, ...) {
  "!stablehlo.token"
}

#' @export
print.TokenType <- function(x, ...) {
  cat("<TokenType: ", repr(x), ">\n", sep = "")
  invisible(x)
}

#' @title ValueType
#' @description
#' This represents the type of a value.
#' @param type The type of the value (TensorType or TokenType).
#' @param shape The shape of the value (only used when type is character).
#' @export
ValueType <- function(type, shape = NULL) {
  if (is.character(type)) {
    return(make_vt(type, shape = shape))
  }

  # Validate type is TensorType or TokenType
  if (
    !test_class(type, "TensorType") &&
      !test_class(type, "TokenType")
  ) {
    cli_abort("type must be a TensorType or TokenType")
  }

  structure(
    list(type = type),
    class = "ValueType"
  )
}

#' @export
#' @method dtype ValueType
dtype.ValueType <- function(x, ...) {
  if (test_class(x$type, "TensorType")) {
    x$type$dtype
  } else if (test_class(x$type, "TokenType")) {
    stop("ValueType with TokenType has no dtype")
  } else {
    stop("Unsupported ValueType for dtype")
  }
}

#' @export
#' @method shape ValueType
shape.ValueType <- function(x, ...) {
  shape(x$type)
}

#' @export
repr.ValueType <- function(x, ...) {
  repr(x$type)
}

#' @export
print.ValueType <- function(x, ...) {
  cat("<ValueType: ", repr(x), ">\n", sep = "")
  invisible(x)
}

#' @exportS3Method cli::cli_format
cli_format.ValueType <- function(x, style = NULL, ...) {
  repr(x)
}

make_vt <- function(type, shape) {
  if (type == "token") {
    return(ValueType(TokenType()))
  }
  ValueType(TensorType(as_dtype(type), Shape(shape)))
}

#' @export
`==.ValueType` <- function(e1, e2) {
  if (!test_class(e2, "ValueType")) {
    return(FALSE)
  }
  e1$type == e2$type
}

#' @export
`!=.ValueType` <- function(e1, e2) {
  !(e1 == e2) # nolint
}

#' @export
repr.ValueType <- function(x, ...) {
  repr(x$type)
}

#' @title ValueTypes
#' @description
#' List of [`ValueType`]s.
#' @param items (`list()` of [`ValueType`])\cr
#'   The types of the values.
#' @return `ValueTypes`
#' @export
ValueTypes <- new_list_of("ValueTypes", "ValueType")

#' @export
repr.ValueTypes <- function(x, ...) {
  paste0(
    vapply(x, repr, character(1)),
    collapse = ", "
  )
}

#' @export
print.ValueTypes <- function(x, ...) {
  n <- length(x)
  if (n == 0) {
    cat("<ValueTypes: (empty)>\n")
  } else if (n == 1) {
    cat("<ValueTypes: ", repr(x), ">\n", sep = "")
  } else {
    cat("<ValueTypes[", n, "]>\n", sep = "")
    for (i in seq_along(x)) {
      cat("  [", i, "] ", repr(x[[i]]), "\n", sep = "")
    }
  }
  invisible(x)
}

check_types_equal <- function(lhs, rhs, ..., msg = NULL) {
  rlang::check_dots_empty()

  if (lhs == rhs) {
    return()
  }

  cli_abort(c(
    x = msg %||% "Expected types to be equal.",
    x = "Got lhs={.val {lhs}}, rhs={.val {rhs}}."
  ))
}
