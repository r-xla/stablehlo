#' @include shape.R
NULL

#' TensorDataType Base Class
#'
#' @description
#' `TensorDataType` is the parent S3 class for all tensor data types.
#' All data type classes inherit from `TensorDataType`, enabling cross-type
#' comparisons with `==` and `!=` operators.
#'
#' The specific data type classes are:
#' - [BooleanType()] - Boolean (i1)
#' - [IntegerType()] - Signed integers (i8, i16, i32, i64)
#' - [UnsignedType()] - Unsigned integers (ui8, ui16, ui32, ui64)
#' - [FloatType()] - Floating point (f32, f64)
#'
#' @details
#' This is a virtual base class - you cannot create instances directly.
#' Use the specific type constructors instead.
#'
#' @seealso [BooleanType()], [IntegerType()], [UnsignedType()], [FloatType()]
#' @name TensorDataType
NULL

#' @export
`==.TensorDataType` <- function(e1, e2) {
  # If classes don't match, types are not equal
  if (!identical(class(e1)[1], class(e2)[1])) {
    return(FALSE)
  }

  # BooleanType has no value field
  if (inherits(e1, "BooleanType")) {
    return(TRUE)
  }

  # For IntegerType, UnsignedType, FloatType - compare values
  identical(e1$value, e2$value)
}

#' @exportS3Method cli::cli_format
cli_format.TensorDataType <- function(x, style = NULL, ...) {
  repr(x)
}

#' @export
`!=.TensorDataType` <- function(e1, e2) {
  # If classes don't match, types are not equal
  if (!identical(class(e1)[1], class(e2)[1])) {
    return(TRUE)
  }

  # BooleanType has no value field
  if (inherits(e1, "BooleanType")) {
    return(FALSE)
  }

  # For IntegerType, UnsignedType, FloatType - compare values
  !identical(e1$value, e2$value)
}

#' @title BooleanType
#' @description
#' Represents the boolean type.
#' @return `BooleanType`
#' @export
BooleanType <- function() {
  structure(list(), class = c("BooleanType", "TensorDataType"))
}

#' @export
repr.BooleanType <- function(x, ...) {
  "i1"
}

#' @export
print.BooleanType <- function(x, ...) {
  cat("<BooleanType>\n")
  invisible(x)
}

#' @title IntegerType (signed)
#' @description
#' Represents a signed integer type with a given bit width.
#' @param value (`integer(1)`)
#' @return `IntegerType`
#' @export
IntegerType <- function(value) {
  value <- as.integer(value)
  checkmate::assert_int(value)
  if (!(value %in% c(8L, 16L, 32L, 64L))) {
    cli_abort("Unsupported signed integer bit width: {value}")
  }

  structure(
    list(value = value),
    class = c("IntegerType", "TensorDataType")
  )
}

#' @export
repr.IntegerType <- function(x, ...) {
  paste0("i", x$value)
}

#' @export
print.IntegerType <- function(x, ...) {
  cat(sprintf("<IntegerType: %s>\n", x$value))
  invisible(x)
}

#' @title UnsignedType
#' @description
#' Represents an unsigned integer type with a given bit width.
#' @param value (`integer(1)`)
#' @return `UnsignedType`
#' @export
UnsignedType <- function(value) {
  value <- as.integer(value)
  checkmate::assert_int(value)
  if (!(value %in% c(8L, 16L, 32L, 64L))) {
    cli_abort("Unsupported unsigned integer bit width: {value}")
  }

  structure(
    list(value = value),
    class = c("UnsignedType", "TensorDataType")
  )
}

#' @export
repr.UnsignedType <- function(x, ...) {
  paste0("ui", x$value)
}

#' @export
print.UnsignedType <- function(x, ...) {
  cat(sprintf("<UnsignedType: %s>\n", x$value))
  invisible(x)
}

#' @title FloatType
#' @description
#' Represents a floating point type with a given bit width.
#' @param value (`integer(1)`)
#' @return `FloatType`
#' @export
FloatType <- function(value) {
  value <- as.integer(value)
  checkmate::assert_int(value)
  if (!(value %in% c(32L, 64L))) {
    cli_abort("Unsupported float bit width: {value}")
  }

  structure(
    list(value = value),
    class = c("FloatType", "TensorDataType")
  )
}

#' @export
repr.FloatType <- function(x, ...) {
  paste0("f", x$value)
}

#' @export
print.FloatType <- function(x, ...) {
  cat(sprintf("<FloatType: %s>\n", x$value))
  invisible(x)
}


#' @title Is TensorDataType
#' @description
#' Check if an object is a [`TensorDataType`].
#' @param x (any)\cr
#'   Object to check.
#' @return `logical(1)`
#' @export
is_dtype <- function(x) {
  inherits(x, "TensorDataType")
}

# Helper to check if something is a valid TensorDataType
assert_dtype <- function(x, arg = rlang::caller_arg(x)) {
  if (!is_dtype(x)) {
    cli_abort(
      "{.arg {arg}} must be a TensorDataType (BooleanType, IntegerType, UnsignedType, or FloatType)"
    )
  }
}


#' @title Convert to TensorDataType
#' @description
#' Convert to TensorDataType.
#' @param x (any)\cr
#'   Object to convert.
#'   Can currently be a string (one of `r roxy_dtypes()`) or a [`TensorDataType`] object.
#' @return `TensorDataType`
#' @export
as_dtype <- function(x) {
  UseMethod("as_dtype")
}

#' @export
as_dtype.default <- function(x) {
  cli_abort("Cannot convert {.cls {class(x)[1]}} to TensorDataType")
}


#' @export
as_dtype.TensorDataType <- function(x) {
  x
}

dtype_map <- list(
  "pred" = BooleanType(),
  "i1" = BooleanType(),
  "i8" = IntegerType(8L),
  "i16" = IntegerType(16L),
  "i32" = IntegerType(32L),
  "i64" = IntegerType(64L),
  "ui8" = UnsignedType(8L),
  "ui16" = UnsignedType(16L),
  "ui32" = UnsignedType(32L),
  "ui64" = UnsignedType(64L),
  "f32" = FloatType(32L),
  "f64" = FloatType(64L)
)

#' @export
as_dtype.character <- function(x) {
  dtype_map[[x]] %??% cli_abort("Unsupported dtype: {x}")
}

#' @export
as.character.BooleanType <- function(x, ...) {
  "i1"
}

#' @export
as.character.IntegerType <- function(x, ...) {
  repr(x)
}

#' @export
as.character.UnsignedType <- function(x, ...) {
  repr(x)
}

#' @export
as.character.FloatType <- function(x, ...) {
  repr(x)
}

#' @title TensorType
#' @description
#' Represents a tensor type with a specific data type and shape.
#' @param dtype ([`TensorDataType`])
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
  paste0(
    "tensor<",
    repr(x$shape),
    if (length(x$shape$dims) > 0) "x" else "",
    repr(x$dtype),
    ">"
  )
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
    x = "Got lhs={.val {repr(lhs)}}, rhs={.val {repr(rhs)}}."
  ))
}
