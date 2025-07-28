#' @include constant.R
#' @include types.R
NULL

# Helper function to convert PjRT element type string to StableHLO type
string_to_type <- function(element_type) {
  if (is.null(element_type)) {
    return(NULL)
  }

  switch(element_type,
    # Boolean types
    "pred" = BooleanType(),

    # Signed integer types
    "s8" = IntegerType("si8"),
    "s16" = IntegerType("si16"),
    "s32" = IntegerType("si32"),
    "s64" = IntegerType("si64"),

    # Unsigned integer types
    "u8" = IntegerType("ui8"),
    "u16" = IntegerType("ui16"),
    "u32" = IntegerType("ui32"),
    "u64" = IntegerType("ui64"),

    # Floating point types
    "f32" = FloatType("f32"),
    "f64" = FloatType("f64"),

    # Default case
    stop("Unsupported element type: ", element_type)
  )
}

# Create a Constant from R value
r_to_constant <- S7::new_generic("r_to_constant", "value", function(value, element_type = NULL, ...) {
  S7::S7_dispatch()
})

method(r_to_constant, S7::class_logical) <- function(value, element_type = NULL, ...) {
  # For logical values, create a BooleanConstant
  boolean_constant <- BooleanConstant(value)

  shape <- Shape(dim(value) %||% length(value))
  stablehlo_type <- string_to_type(element_type) %||% BooleanType()
  element_type_obj <- TensorElementType(type = stablehlo_type)
  tensor_type <- TensorType(dtype = element_type_obj, shape = shape)

  # Create TensorConstant with the data and type
  tensor_constant <- TensorConstant(
    data = value,
    type = tensor_type
  )

  return(Constant(value = tensor_constant))
}

method(r_to_constant, S7::class_numeric) <- function(value, element_type = NULL, ...) {
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
  float_type <- string_to_type(element_type) %||% FloatType("f32")
  float_constant <- FloatConstant(literal = f, type = float_type)

  shape <- Shape(dim(value) %||% length(value))

  element_type_obj <- TensorElementType(type = float_type)
  tensor_type <- TensorType(dtype = element_type_obj, shape = shape)

  # Create TensorConstant with the data and type
  tensor_constant <- TensorConstant(
    data = value,
    type = tensor_type
  )

  return(Constant(value = tensor_constant))
}

method(r_to_constant, S7::class_integer) <- function(value, element_type = NULL, ...) {
  # For integer values, create an IntegerConstant
  # Use provided element_type or default to si64
  integer_type <- string_to_type(element_type) %||% IntegerType("si64")
  integer_constant <- IntegerConstant(value = value, type = integer_type)

  shape <- Shape(dim(value) %||% length(value))

  element_type_obj <- TensorElementType(type = integer_type)
  tensor_type <- TensorType(dtype = element_type_obj, shape = shape)

  # Create TensorConstant with the data and type
  tensor_constant <- TensorConstant(
    data = value,
    type = tensor_type
  )

  return(Constant(value = tensor_constant))
}

method(r_to_constant, S7::class_any) <- function(value, element_type = NULL, ...) {
  stop("Unsupported type for r_to_constant: ", class(value)[1])
}

assert_one_of <- function(x, ...) {
  for (type in list(...)) {
    if (inherits(x, type)) {
      return(TRUE)
    }
  }
  stop("Invalid type")
}

# takes in a body and returns the output types of the last instruction
output_types_from_body <- function(body) {
  body@items[[length(body@items)]]@inputs@values
}

#' Convert R value to StableHLO string representation
#' @param value The R value to convert
#' @return A string representation suitable for StableHLO dense format
r_to_stablehlo_string <- function(value) {
  if (is.logical(value)) {
    # Boolean data
    if (value) {
      return("true")
    } else {
      return("false")
    }
  } else if (is.integer(value)) {
    # Integer data
    return(as.character(value))
  } else if (is.numeric(value)) {
    # Float data - format with scientific notation
    value_str <- formatC(value, digits = 16, format = "e")
    if (value >= 0) {
      return(paste0("+", value_str))
    } else {
      return(value_str)
    }
  } else {
    # Fallback
    return(as.character(value))
  }
}
