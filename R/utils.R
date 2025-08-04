#' @include constant.R
#' @include types.R
NULL

# Helper function to convert PjRT element type string to StableHLO type
string_to_type <- function(element_type) {
  if (is.null(element_type)) {
    return(NULL)
  }

  switch(
    element_type,
    # Boolean types
    "pred" = BooleanType(),

    # Signed integer types
    "i8" = IntegerType("i8"),
    "i16" = IntegerType("i16"),
    "i32" = IntegerType("i32"),
    "i64" = IntegerType("i64"),

    # Unsigned integer types
    "ui8" = IntegerType("ui8"),
    "ui16" = IntegerType("ui16"),
    "ui32" = IntegerType("ui32"),
    "ui64" = IntegerType("ui64"),

    # Floating point types
    "f32" = FloatType("f32"),
    "f64" = FloatType("f64"),

    # Default case
    stop("Unsupported element type: ", element_type)
  )
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
r_to_stablehlo_string <- function(value, dtype) {
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
    if (dtype == "f32") {
      format_double(value, precision = 32)
    } else {
      format_double(value, precision = 64)
    }
  } else {
    # Fallback
    return(as.character(value))
  }
}
