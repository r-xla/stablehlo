#' @include shape.R
NULL

#' @export
tengen::is_dtype

#' @export
tengen::as_dtype

# Re-export assert_dtype from tengen
assert_dtype <- tengen::assert_dtype

# Dtypes whose MLIR spelling differs from their canonical tengen name.
mlir_dtype_overrides <- c(
  bool = "i1",
  c64 = "complex<f32>",
  c128 = "complex<f64>"
)

# MLIR spelling of a DataType.
dtype_str <- function(dtype) {
  name <- as.character(dtype)
  spelling <- mlir_dtype_overrides[name]
  if (is.na(spelling)) name else unname(spelling)
}

#' @export
repr.DataType <- function(x, ...) {
  dtype_str(x)
}

#' @title TensorType
#' @description
#' Represents a tensor type with a specific data type and shape.
#' @param dtype ([`DataType`])
#' @param shape ([`Shape`])
#' @return `TensorType`
#' @export
TensorType <- function(dtype, shape) {
  dims <- shape$dims
  if (anyNA(dims)) {
    dims[is.na(dims)] <- "?"
  }

  # The rendered type string is cached; it uniquely encodes (dtype, shape),
  # so repr and equality never have to re-derive it.
  str <- if (length(dims) > 0L) {
    paste0("tensor<", paste0(dims, collapse = "x"), "x", dtype_str(dtype), ">")
  } else {
    paste0("tensor<", dtype_str(dtype), ">")
  }
  structure(
    list(dtype = dtype, shape = shape, str = str),
    class = "TensorType"
  )
}

#' @export
`==.TensorType` <- function(e1, e2) {
  inherits(e2, "TensorType") && identical(e1$str, e2$str)
}

#' @export
# jarl-ignore comparison_negation: != must delegate to == for S3 consistency
`!=.TensorType` <- function(e1, e2) {
  !(e1 == e2) # nolint
}

#' @export
repr.TensorType <- function(x, ...) {
  x$str
}

#' @export
print.TensorType <- function(x, ...) {
  cat(repr(x), "\n")
  invisible(x)
}

#' @exportS3Method cli::cli_format
cli_format.TensorType <- function(x, style = NULL, ...) {
  repr(x)
}

#' @export
#' @method shape TensorType
shape.TensorType <- function(x, ...) {
  x$shape$dims
}

#' @export
#' @method dtype TensorType
dtype.TensorType <- function(x, ...) {
  x$dtype
}

# TokenType - internal, not exported
TokenType <- function() {
  structure(list(str = "!stablehlo.token"), class = "TokenType")
}

#' @export
repr.TokenType <- function(x, ...) {
  x$str
}

# Cached type string of a ValueType (both TensorType and TokenType carry one).
type_str <- function(vt) {
  vt$type$str
}

#' @export
print.TokenType <- function(x, ...) {
  cat("<TokenType: ", repr(x), ">\n", sep = "")
  invisible(x)
}

#' @title ValueType
#' @description
#' This represents the type of a value.
#' @param type The type of the value (TensorType or TokenType).
#' @param shape The shape of the value (only used when type is character).
#' @export
ValueType <- function(type, shape = NULL) {
  # A DataType is itself a character vector, so exclude it from the
  # dtype-string shortcut.
  if (is.character(type) && !is_dtype(type)) {
    return(make_vt(type, shape = shape))
  }

  # Validate type is TensorType or TokenType
  if (!inherits(type, "TensorType") && !inherits(type, "TokenType")) {
    cli_abort("type must be a TensorType or TokenType")
  }

  structure(
    list(type = type),
    class = "ValueType"
  )
}

#' @export
#' @method dtype ValueType
dtype.ValueType <- function(x, ...) {
  if (inherits(x$type, "TensorType")) {
    x$type$dtype
  } else if (inherits(x$type, "TokenType")) {
    stop("ValueType with TokenType has no dtype")
  } else {
    stop("Unsupported ValueType for dtype")
  }
}

#' @export
#' @method shape ValueType
shape.ValueType <- function(x, ...) {
  x$type$shape$dims
}

#' @export
repr.ValueType <- function(x, ...) {
  repr(x$type)
}

#' @export
print.ValueType <- function(x, ...) {
  cat("<ValueType: ", repr(x), ">\n", sep = "")
  invisible(x)
}

make_vt <- function(type, shape) {
  if (type == "token") {
    return(ValueType(TokenType()))
  }
  ValueType(TensorType(as_dtype(type), Shape(shape)))
}

#' @export
`==.ValueType` <- function(e1, e2) {
  if (!inherits(e2, "ValueType")) {
    return(FALSE)
  }
  e1$type == e2$type
}

#' @export
# jarl-ignore comparison_negation: != must delegate to == for S3 consistency
`!=.ValueType` <- function(e1, e2) {
  !(e1 == e2) # nolint
}

#' @title ValueTypes
#' @description
#' List of [`ValueType`]s.
#' @param items (`list()` of [`ValueType`])\cr
#'   The types of the values.
#' @return `ValueTypes`
#' @export
ValueTypes <- new_list_of("ValueTypes", "ValueType")

#' @export
repr.ValueTypes <- function(x, ...) {
  paste0(
    vapply(x, type_str, character(1)),
    collapse = ", "
  )
}

#' @export
print.ValueTypes <- function(x, ...) {
  n <- length(x)
  if (n == 0) {
    cat("<ValueTypes: (empty)>\n")
  } else if (n == 1) {
    cat("<ValueTypes: ", repr(x), ">\n", sep = "")
  } else {
    cat("<ValueTypes[", n, "]>\n", sep = "")
    for (i in seq_along(x)) {
      cat("  [", i, "] ", repr(x[[i]]), "\n", sep = "")
    }
  }
  invisible(x)
}
