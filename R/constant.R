#' @include types.R
#' @include repr.R
NULL

SignPart <- new_enum("SignPart", c("+", "-"))
method(repr, SignPart) <- function(x) {
  as.character(x@Value)
}

decimalDigit <- new_enum("DecimalDigit", 0:9)
method(repr, decimalDigit) <- function(x) {
  as.character(x@Value)
}

DecimalDigits <- new_list_of(
  "DecimalDigits",
  item_type = decimalDigit
)
method(repr, DecimalDigits) <- function(x) {
  paste0(sapply(x@items, repr), collapse = "")
}


IntegerPart <- new_class("IntegerPart", parent = DecimalDigits)
FractionalPart <- new_class("FractionalPart", parent = DecimalDigits)
method(repr, FractionalPart) <- function(x) {
  paste0(".", paste0(sapply(x@items, repr), collapse = ""))
}

ScientificPart <- new_class("ScientificPart", properties = list(
  exponent_sign = SignPart,
  exponent_digits = DecimalDigits
))

method(repr, ScientificPart) <- function(x) {
  paste0(
    "E",
    repr(x@exponent_sign),
    repr(x@exponent_digits)
  )
}

FloatLiteral <- new_class(
  "FloatLiteral",
  properties = list(
    sign_part = SignPart,
    integer_part = IntegerPart,
    fractional_part = FractionalPart,
    scientific_part = ScientificPart
  )
)

method(repr, FloatLiteral) <- function(x) {
  paste0(
    repr(x@sign_part),
    repr(x@integer_part),
    repr(x@fractional_part),
    repr(x@scientific_part)
  )
}

ElementLiteral <- new_class(
  "ElementLiteral",
  properties = list(
    literal = FloatLiteral
  )
)

method(repr, ElementLiteral) <- function(x) {
  repr(x@literal)
}

TensorLiteral <- new_class(
  "TensorLiteral",
  properties = list(
    literal = ElementLiteral
  )
)

method(repr, TensorLiteral) <- function(x) {
  paste0("dense<", repr(x@literal), ">")
}

TensorConstant <- new_class(
  "TensorConstant",
  properties = list(
    literal = TensorLiteral,
    type = TensorType
  )
)

method(repr, TensorConstant) <- function(x) {
  paste0(
    repr(x@literal),
    " : ",
    repr(x@type)
  )
}

Constant <- new_class("Constant", properties = list(
  value = TensorConstant
))

method(repr, Constant) <- function(x) {
  repr(x@value)
}

