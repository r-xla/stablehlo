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

ScientificPart <- new_class(
  "ScientificPart",
  properties = list(
    exponent_sign = SignPart,
    exponent_digits = DecimalDigits
  )
)

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
    if (x@sign_part@Value == "-") "-" else "",
    repr(x@integer_part),
    repr(x@fractional_part),
    repr(x@scientific_part)
  )
}

BooleanConstant <- new_class(
  "BooleanConstant",
  properties = list(
    literal = S7::class_logical
  )
)

method(repr, BooleanConstant) <- function(x) {
  if (x@literal) {
    "true"
  } else {
    "false"
  }
}

IntegerConstant <- new_class(
  "IntegerConstant",
  properties = list(
    value = S7::class_integer,
    type = IntegerType
  )
)

method(repr, IntegerConstant) <- function(x) {
  as.character(x@value)
}

FloatConstant <- new_class(
  "FloatConstant",
  properties = list(
    literal = FloatLiteral,
    type = FloatType
  )
)

method(repr, FloatConstant) <- function(x) {
  repr(x@literal)
}

TensorLiteral <- new_class(
  "TensorLiteral",
  properties = list(
    literal = S7::new_union(
      BooleanConstant,
      IntegerConstant,
      FloatConstant
    )
  )
)

method(repr, TensorLiteral) <- function(x) {
  paste0("dense<", repr(x@literal), ">")
}

TensorConstant <- new_class(
  "TensorConstant",
  properties = list(
    data = S7::class_any,
    type = TensorType
  )
)

method(repr, TensorConstant) <- function(x) {
  # Generate proper StableHLO dense format
  data <- x@data
  type <- x@type

  # Convert data to StableHLO string representation
  value_str <- r_to_stablehlo_string(data)

  paste0("dense<", value_str, "> : ", repr(type))
}

Constant <- new_class(
  "Constant",
  properties = list(
    value = S7::new_union(
      BooleanConstant,
      IntegerConstant,
      FloatConstant,
      TensorConstant
    )
  )
)

method(repr, Constant) <- function(x) {
  repr(x@value)
}

# Create a Constant from R value
r_to_constant <- S7::new_generic(
  "r_to_constant",
  "value",
  function(value, elt_type = NULL, ...) {
    S7::S7_dispatch()
  }
)

method(r_to_constant, S7::class_logical) <- function(
  value,
  elt_type = NULL,
  ...
) {
  # For logical values, create a BooleanConstant
  boolean_constant <- BooleanConstant(value)

  shape <- Shape(integer()) #Shape(dim(value) %||% length(value))
  stablehlo_type <- BooleanType()
  element_type_obj <- TensorElementType(type = stablehlo_type)
  tensor_type <- TensorType(dtype = element_type_obj, shape = shape)

  # Create TensorConstant with the data and type
  tensor_constant <- TensorConstant(
    data = value,
    type = tensor_type
  )

  return(Constant(value = tensor_constant))
}

method(r_to_constant, S7::class_numeric) <- function(
  value,
  elt_type = NULL,
  ...
) {
  # For numeric values, create a FloatConstant
  x <- formatC(abs(value), digits = 16, format = "e")

  parts <- strcapture(
    "([0-9]+)\\.([0-9]+)e([+-])([0-9]+)",
    x,
    proto = list(
      integral = character(),
      fractional = character(),
      exponent_sign = character(),
      exponent_digits = character()
    )
  )

  charpart_to_digit <- \(x) {
    x |> strsplit("") |> unlist() |> as.integer() |> lapply(decimalDigit)
  }

  integer_part <- charpart_to_digit(parts$integral) |> IntegerPart()
  fractional_part <- charpart_to_digit(parts$fractional) |> FractionalPart()
  exponent_sign <- SignPart(parts$exponent_sign)
  exponent_digits <- charpart_to_digit(parts$exponent_digits) |> IntegerPart()

  f <- FloatLiteral(
    sign_part = SignPart(if (value >= 0) "+" else "-"),
    integer_part = integer_part,
    fractional_part = fractional_part,
    scientific_part = ScientificPart(
      exponent_sign = exponent_sign,
      exponent_digits = exponent_digits
    )
  )

  # Use provided element_type or default to f32
  elt_type <- if (is.null(elt_type)) {
    FloatType("f32")
  } else {
    string_to_type(elt_type)
  }
  float_constant <- FloatConstant(literal = f, type = elt_type)

  shape <- Shape(integer()) #Shape(dim(value) %||% length(value))

  element_type_obj <- TensorElementType(type = elt_type)
  tensor_type <- TensorType(dtype = element_type_obj, shape = shape)

  # Create TensorConstant with the data and type
  tensor_constant <- TensorConstant(
    data = value,
    type = tensor_type
  )

  return(Constant(value = tensor_constant))
}

method(r_to_constant, S7::class_integer) <- function(
  value,
  elt_type = NULL,
  ...
) {
  # For integer values, create an IntegerConstant
  # Use provided element_type or default to i64
  elt_type <- if (is.null(elt_type)) {
    IntegerType("i64")
  } else {
    string_to_type(elt_type)
  }
  integer_constant <- IntegerConstant(value = value, type = elt_type)

  shape <- Shape(integer()) #Shape(dim(value) %||% length(value))

  element_type_obj <- TensorElementType(type = elt_type)
  tensor_type <- TensorType(dtype = element_type_obj, shape = shape)

  # Create TensorConstant with the data and type
  tensor_constant <- TensorConstant(
    data = value,
    type = tensor_type
  )

  return(Constant(value = tensor_constant))
}

method(r_to_constant, S7::class_any) <- function(
  value,
  elt_type = NULL,
  ...
) {
  stop("Unsupported type for r_to_constant: ", class(value)[1])
}
