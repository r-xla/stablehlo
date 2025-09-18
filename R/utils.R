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

#' @title Represent an R value in stableHLO
#' @description
#' Convert R value to StableHLO string representation
#' @param value (any)\cr
#'  The R value to convert
#' @param dtype (`character(1)`)\cr
#'   The element type to use.
#' @return `character(1)`
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

snake_to_camel <- function(str) {
  paste(capitalize(strsplit(str, "_")[[1]]), collapse = "")
}

capitalize <- function(str) {
  substr(str, 1L, 1L) <- toupper(substr(str, 1L, 1L))
  str
}

get_dims <- function(data) {
  if (is.null(dim(data))) {
    if (length(data) == 1) {
      return(1L)
    }
    return(length(data))
  }
  dim(data)
}

restore_previous_func <- function() {
  stash_size <- length(globals[["FUNC_STASH"]])
  if (stash_size) {
    globals[["CURRENT_FUNC"]] <- globals[["FUNC_STASH"]][[stash_size]]
    globals[["FUNC_STASH"]] <- globals[["FUNC_STASH"]][-stash_size]
  } else {
    globals[["CURRENT_FUNC"]] <- NULL
  }
}
