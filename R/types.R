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
  structure(list(), class = "stablehlo_BooleanType")
}

#' @export
repr.stablehlo_BooleanType <- function(x, ...) {
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
    class = "stablehlo_IntegerType"
  )
}

#' @export
repr.stablehlo_IntegerType <- function(x, ...) {
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
    class = "stablehlo_UnsignedType"
  )
}

#' @export
repr.stablehlo_UnsignedType <- function(x, ...) {
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
    class = "stablehlo_FloatType"
  )
}

#' @export
repr.stablehlo_FloatType <- function(x, ...) {
  paste0("f", x$value)
}

#' @title TensorDataType
#' @description
#' Type union of all possible data types.
#' Not a class in S3, just documentation that dtype can be BooleanType, IntegerType, UnsignedType, or FloatType.
#' @export
TensorDataType <- c(
  "stablehlo_BooleanType",
  "stablehlo_IntegerType",
  "stablehlo_UnsignedType",
  "stablehlo_FloatType"
)

#' @title Is TensorDataType
#' @description
#' Check if an object is a [`TensorDataType`].
#' @param x (any)\cr
#'   Object to check.
#' @return `logical(1)`
#' @export
is_dtype <- function(x) {
  inherits(x, "stablehlo_IntegerType") ||
    inherits(x, "stablehlo_UnsignedType") ||
    inherits(x, "stablehlo_FloatType") ||
    inherits(x, "stablehlo_BooleanType")
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
`==.stablehlo_BooleanType` <- function(e1, e2) {
  inherits(e2, "stablehlo_BooleanType")
}

#' @export
`!=.stablehlo_BooleanType` <- function(e1, e2) {
  !(e1 == e2)
}

#' @export
`==.stablehlo_IntegerType` <- function(e1, e2) {
  inherits(e2, "stablehlo_IntegerType") && e1$value == e2$value
}

#' @export
`!=.stablehlo_IntegerType` <- function(e1, e2) {
  !(e1 == e2)
}

#' @export
`==.stablehlo_UnsignedType` <- function(e1, e2) {
  inherits(e2, "stablehlo_UnsignedType") && e1$value == e2$value
}

#' @export
`!=.stablehlo_UnsignedType` <- function(e1, e2) {
  !(e1 == e2)
}

#' @export
`==.stablehlo_FloatType` <- function(e1, e2) {
  inherits(e2, "stablehlo_FloatType") && e1$value == e2$value
}

#' @export
`!=.stablehlo_FloatType` <- function(e1, e2) {
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

#' @export
as_dtype.character <- function(x) {
  if (x %in% c("pred", "i1")) {
    return(BooleanType())
  }

  if (grepl("^i[0-9]+$", x)) {
    return(IntegerType(as.integer(sub("^i", "", x))))
  }
  if (grepl("^ui[0-9]+$", x)) {
    return(UnsignedType(as.integer(sub("^ui", "", x))))
  }
  if (grepl("^f[0-9]+$", x)) {
    return(FloatType(as.integer(sub("^f", "", x))))
  }
  cli_abort("Unsupported dtype: {x}")
}

#' @export
as.character.stablehlo_BooleanType <- function(x, ...) {
  "i1"
}

#' @export
as.character.stablehlo_IntegerType <- function(x, ...) {
  repr(x)
}

#' @export
as.character.stablehlo_UnsignedType <- function(x, ...) {
  repr(x)
}

#' @export
as.character.stablehlo_FloatType <- function(x, ...) {
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
  checkmate::assert_class(shape, "stablehlo_Shape")

  structure(
    list(dtype = dtype, shape = shape),
    class = "stablehlo_TensorType"
  )
}

# Helper for dtype comparison that handles cross-class comparisons
dtypes_equal <- function(d1, d2) {
  c1 <- class(d1)[1]
  c2 <- class(d2)[1]
  if (c1 != c2) {
    return(FALSE)
  }
  if (c1 == "stablehlo_BooleanType") {
    return(TRUE)
  }
  # IntegerType, UnsignedType, FloatType all have $value
  d1$value == d2$value
}

#' @export
`==.stablehlo_TensorType` <- function(e1, e2) {
  inherits(e2, "stablehlo_TensorType") &&
    dtypes_equal(e1$dtype, e2$dtype) &&
    e1$shape == e2$shape
}

#' @export
`!=.stablehlo_TensorType` <- function(e1, e2) {
  !(e1 == e2)
}

#' @export
repr.stablehlo_TensorType <- function(x, ...) {
  paste0(
    "tensor<",
    repr(x$shape),
    if (length(x$shape$dims) > 0) "x" else "",
    repr(x$dtype),
    ">"
  )
}

#' @export
#' @method shape stablehlo_TensorType
shape.stablehlo_TensorType <- function(x, ...) {
  x$shape$dims
}

#' @export
#' @method dtype stablehlo_TensorType
dtype.stablehlo_TensorType <- function(x, ...) {
  x$dtype
}

# TokenType - internal, not exported
TokenType <- function() {
  structure(list(), class = "stablehlo_TokenType")
}

#' @export
repr.stablehlo_TokenType <- function(x, ...) {
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
    !inherits(type, "stablehlo_TensorType") &&
      !inherits(type, "stablehlo_TokenType")
  ) {
    cli_abort("type must be a TensorType or TokenType")
  }

  structure(
    list(type = type),
    class = "stablehlo_ValueType"
  )
}

#' @export
#' @method dtype stablehlo_ValueType
dtype.stablehlo_ValueType <- function(x, ...) {
  if (inherits(x$type, "stablehlo_TensorType")) {
    x$type$dtype
  } else if (inherits(x$type, "stablehlo_TokenType")) {
    stop("ValueType with TokenType has no dtype")
  } else {
    stop("Unsupported ValueType for dtype")
  }
}

#' @export
#' @method shape stablehlo_ValueType
shape.stablehlo_ValueType <- function(x, ...) {
  shape(x$type)
}

make_vt <- function(type, shape) {
  if (type == "token") {
    return(ValueType(TokenType()))
  }
  ValueType(TensorType(as_dtype(type), Shape(shape)))
}

#' @export
`==.stablehlo_ValueType` <- function(e1, e2) {
  if (!inherits(e2, "stablehlo_ValueType")) {
    return(FALSE)
  }
  e1$type == e2$type
}

#' @export
`!=.stablehlo_ValueType` <- function(e1, e2) {
  !(e1 == e2)
}

#' @export
repr.stablehlo_ValueType <- function(x, ...) {
  repr(x$type)
}

#' @title ValueTypes
#' @description
#' List of [`ValueType`]s.
#' @param items (`list()` of [`ValueType`])\cr
#'   The types of the values.
#' @return `ValueTypes`
#' @export
ValueTypes <- new_list_of("stablehlo_ValueTypes", "stablehlo_ValueType")

#' @export
repr.stablehlo_ValueTypes <- function(x, ...) {
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
