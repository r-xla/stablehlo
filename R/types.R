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
    "si2",
    "si4",
    "si8",
    "si16",
    "si32",
    "si64",
    "ui2",
    "ui4",
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
    "f4E2M1FN",
    "f6E2M3FN",
    "f6E3M2FN",
    "f8E3M4",
    "f8E4M3",
    "f8E4M3FN",
    "f8E4M3FNUZ",
    "f8E4M3B11FNUZ",
    "f8E5M2",
    "f8E5M2FNUZ",
    "f8E8M0FNU",
    "bf16",
    "f16",
    "f32",
    "f64"
  )
)

ComplexType <- new_class("ComplexType")

TensorElementType <- new_class(
  name = "TensorElementType",
  properties = list(
    type = S7::new_union(
      BooleanType,
      IntegerType,
      FloatType,
      ComplexType
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
  FloatType,
  ComplexType
)

method(`==`, list(element_type_union, element_type_union)) <- function(e1, e2) {
  if (!identical(S7::S7_class(e1), S7::S7_class(e2))) {
    return(FALSE)
  }
  if (inherits(e1, BooleanType)) {
    return(TRUE)
  }
  if (inherits(e1, ComplexType)) {
    .NotYetImplemented()
  }
  # Float and Int are both enums
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
    "x",
    repr(x@dtype),
    ">"
  )
}

TokenType <- new_class("TokenType")

method(repr, TokenType) <- function(x) {
  "!stablehlo.token"
}

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
  )
)

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

method(`==`, list(ValueTypes, ValueTypes)) <- function(e1, e2) {
  length(e1@items) == length(e2@items) &&
    all(sapply(seq_along(e1@items), function(i) {
      e1@items[[i]] == e2@items[[i]]
    }))
}