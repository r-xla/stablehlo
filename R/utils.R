#' @include constant.R
#' @include types.R
NULL

string_to_type <- function(element_type) {
  if (is.null(element_type)) {
    return(NULL)
  }

  switch(
    element_type,
    "pred" = BooleanType(),

    "i8" = IntegerType("i8"),
    "i16" = IntegerType("i16"),
    "i32" = IntegerType("i32"),
    "i64" = IntegerType("i64"),

    "ui8" = IntegerType("ui8"),
    "ui16" = IntegerType("ui16"),
    "ui32" = IntegerType("ui32"),
    "ui64" = IntegerType("ui64"),

    "f32" = FloatType("f32"),
    "f64" = FloatType("f64"),

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

output_types_from_body <- function(body) {
  body@items[[length(body@items)]]@inputs@values
}

#' Convert R value to StableHLO string representation
#' @param value The R value to convert
#' @return A string representation suitable for StableHLO dense format
r_to_stablehlo_string <- function(value, dtype) {
  if (is.logical(value)) {
    if (value) {
      return("true")
    } else {
      return("false")
    }
  } else if (is.integer(value)) {
    return(as.character(value))
  } else if (is.numeric(value)) {
    if (dtype == "f32") {
      format_double(value, precision = 32)
    } else {
      format_double(value, precision = 64)
    }
  } else {
    return(as.character(value))
  }
}
