#' @include enum.R
#' @include shape.R
NULL

BooleanType <- new_class("BooleanType")
method(repr, BooleanType) <- function(x) {
  "i1"
}

IntegerType <- new_class("IntegerType")
FloatType <- new_enum(
  "FloatType",
  c(
    'f4E2M1FN',
    'f6E2M3FN',
    'f6E3M2FN',
    'f8E3M4',
    'f8E4M3',
    'f8E4M3FN',
    'f8E4M3FNUZ',
    'f8E4M3B11FNUZ',
    'f8E5M2',
    'f8E5M2FNUZ',
    'f8E8M0FNU',
    'bf16',
    'f16',
    'f32',
    'f64'
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

method(repr, TensorElementType) <- function(x) {
  repr(x@type)
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
    repr(x@dtype),
    ">"
  )
}

ValueType <- new_class(
  "ValueType",
  properties = list(
    type = TensorType
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