#' @include enum.R
#' @include shape.R
#' @include list_of.R
NULL

#' @title BooleanType
#' @description
#' Represents the boolean type.
#' @return `BooleanType`
#' @export
BooleanType <- function() {
  structure(list(), class = "BooleanType")
}

#' @export
repr.BooleanType <- function(x, ...) {
  "i1"
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
    class = "IntegerType"
  )
}

#' @export
repr.IntegerType <- function(x, ...) {
  paste0("i", x$value)
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
    class = "UnsignedType"
  )
}

#' @export
repr.UnsignedType <- function(x, ...) {
  paste0("ui", x$value)
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
    class = "FloatType"
  )
}

#' @export
repr.FloatType <- function(x, ...) {
  paste0("f", x$value)
}

#' @title TensorDataType
#' @description
#' Type union of all possible data types.
#' Not a class in S3, just documentation that dtype can be BooleanType, IntegerType, UnsignedType, or FloatType.
#' @export
TensorDataType <- c(
  "BooleanType",
  "IntegerType",
  "UnsignedType",
  "FloatType"
)

#' @title Is TensorDataType
#' @description
#' Check if an object is a [`TensorDataType`].
#' @param x (any)\cr
#'   Object to check.
#' @return `logical(1)`
#' @export
is_dtype <- function(x) {
  test_class(x, "IntegerType") ||
    test_class(x, "UnsignedType") ||
    test_class(x, "FloatType") ||
    test_class(x, "BooleanType")
}

# Helper to check if something is a valid TensorDataType
assert_tensor_dtype <- function(x, arg = rlang::caller_arg(x)) {
  if (!is_dtype(x)) {
    cli_abort(
      "{.arg {arg}} must be a TensorDataType (BooleanType, IntegerType, UnsignedType, or FloatType)"
    )
  }
}

#' @export
`==.BooleanType` <- function(e1, e2) {
  test_class(e2, "BooleanType")
}

#' @export
`!=.BooleanType` <- function(e1, e2) {
  !(e1 == e2)
}

#' @export
`==.IntegerType` <- function(e1, e2) {
  test_class(e2, "IntegerType") && e1$value == e2$value
}

#' @export
`!=.IntegerType` <- function(e1, e2) {
  !(e1 == e2)
}

#' @export
`==.UnsignedType` <- function(e1, e2) {
  test_class(e2, "UnsignedType") && e1$value == e2$value
}

#' @export
`!=.UnsignedType` <- function(e1, e2) {
  !(e1 == e2)
}

#' @export
`==.FloatType` <- function(e1, e2) {
  test_class(e2, "FloatType") && e1$value == e2$value
}

#' @export
`!=.FloatType` <- function(e1, e2) {
  !(e1 == e2)
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
  if (is_dtype(x)) {
    return(x)
  }
  cli_abort("Cannot convert {.cls {class(x)[1]}} to TensorDataType")
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
  assert_tensor_dtype(dtype)
  checkmate::assert_class(shape, "Shape")

  structure(
    list(dtype = dtype, shape = shape),
    class = "TensorType"
  )
}

# Helper for dtype comparison that handles cross-class comparisons
dtypes_equal <- function(d1, d2) {
  c1 <- class(d1)[1]
  c2 <- class(d2)[1]
  if (c1 != c2) {
    return(FALSE)
  }
  if (c1 == "BooleanType") {
    return(TRUE)
  }
  # IntegerType, UnsignedType, FloatType all have $value
  d1$value == d2$value
}

#' @export
`==.TensorType` <- function(e1, e2) {
  test_class(e2, "TensorType") &&
    dtypes_equal(e1$dtype, e2$dtype) &&
    e1$shape == e2$shape
}

#' @export
`!=.TensorType` <- function(e1, e2) {
  !(e1 == e2)
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
  !(e1 == e2)
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
    vapply(x$items, repr, character(1)),
    collapse = ", "
  )
}

check_types_equal <- function(lhs, rhs, ..., msg = NULL) {
  rlang::check_dots_empty()

  if (lhs == rhs) {
    return()
  }

  cli_abort(c(
    x = msg %||% "Expected types to be equal.",
    i = "Got lhs={.val {repr(lhs)}}, rhs={.val {repr(rhs)}}."
  ))
}
