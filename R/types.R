#' @include enum.R
#' @include shape.R
#' @include list_of.R
NULL

BooleanType <- new_class("BooleanType")
method(repr, BooleanType) <- function(x) {
  "i1"
}

IntegerType <- new_enum(
  "IntegerType",
  c(
    #"i2",
    #"i4",
    #"u2",
    #"u4",
    "i8",
    "i16",
    "i32",
    "i64",
    "ui8",
    "ui16",
    "ui32",
    "ui64"
  )
)

method(repr, IntegerType) <- function(x) {
  x@Value
}

FloatType <- new_enum(
  "FloatType",
  c(
    #"f4E2M1FN",
    #"f6E2M3FN",
    #"f6E3M2FN",
    #"f8E3M4",
    #"f8E4M3",
    #"f8E4M3FN",
    #"f8E4M3FNUZ",
    #"f8E4M3B11FNUZ",
    #"f8E5M2",
    #"f8E5M2FNUZ",
    #"f8E8M0FNU",
    #"bf16",
    #"f16",
    "f32",
    "f64"
  )
)

TensorElementType <- new_class(
  name = "TensorElementType",
  properties = list(
    type = S7::new_union(
      BooleanType,
      IntegerType,
      FloatType
    )
  )
)

method(`==`, list(TensorElementType, TensorElementType)) <- function(e1, e2) {
  e1@type == e2@type
}

method(repr, TensorElementType) <- function(x) {
  repr(x@type)
}

element_type_union <- S7::new_union(
  BooleanType,
  IntegerType,
  FloatType
)

method(`==`, list(element_type_union, element_type_union)) <- function(e1, e2) {
  if (!identical(S7::S7_class(e1), S7::S7_class(e2))) {
    return(FALSE)
  }
  if (inherits(e1, BooleanType)) {
    return(TRUE)
  }
  e1@Value == e2@Value
}

TensorType <- new_class(
  "TensorType",
  properties = list(
    dtype = TensorElementType,
    shape = Shape
  )
)

method(repr, TensorType) <- function(x) {
  paste0(
    "tensor<",
    repr(x@shape),
    if (length(x@shape@dims) > 0) "x" else "",
    repr(x@dtype),
    ">"
  )
}

TokenType <- new_class("TokenType")

method(repr, TokenType) <- function(x) {
  "!stablehlo.token"
}

#' @title ValueType
#' @description
#' This represents the type of a value.
#' @param type The type of the value.
#' @param shape The shape of the value.
#' @export
ValueType <- new_class(
  "ValueType",
  properties = list(
    type = S7::new_union(
      # Not supported:]
      # - QuantizedTensorType
      # - TupleType (will probably be removed from stablehlo, is only legacy from xla)
      TokenType,
      TensorType
    )
  ),
  constructor = function(type, shape = NULL) {
    if (is.character(type)) {
      return(make_value_type(type, shape = shape))
    }
    new_object(S7::S7_object(), type = type)
  }
)


method(dim, ValueType) <- function(x) {
  shape(x@type)
}

method(dim, TensorType) <- function(x) {
  x@shape@dims
}

method(`==`, list(ValueType, ValueType)) <- function(e1, e2) {
  e1@type == e2@type
}

value_type_union <- S7::new_union(
  TokenType,
  TensorType
)

method(`==`, list(value_type_union, value_type_union)) <- function(e1, e2) {
  if (!identical(S7::S7_class(e1), S7::S7_class(e2))) {
    return(FALSE)
  }
  if (inherits(e1, TokenType)) {
    return(TRUE)
  }
  # TensorType
  e1@dtype == e2@dtype && e1@shape == e2@shape
}

make_value_type <- function(str, shape = NULL) {
  assert_string(str)
  type <- if (str == "token") {
    TokenType()
  } else {
    if (is.null(shape)) {
      shape <- integer(0)
    }
    dtype <- if (str %in% c("pred", "i1")) {
      BooleanType()
    } else if (grepl("^(i|ui)[0-9]+$", str)) {
      IntegerType(str)
    } else if (grepl("^f[0-9]+$", str)) {
      FloatType(str)
    } else {
      .NotYetImplemented()
    }
    TensorType(TensorElementType(dtype), shape = Shape(shape))
  }

  ValueType(type)
}

method(repr, ValueType) <- function(x) {
  repr(x@type)
}

ValueTypes <- new_list_of("ValueTypes", ValueType)
method(repr, ValueTypes) <- function(x) {
  paste0(
    sapply(x@items, repr),
    collapse = ", "
  )
}
